(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
open Tracing

module D = Debug.Make (struct let name = "test_tracing" end)

open D

let () = Printexc.record_backtrace true

let _ =
  create ~enabled:false ~name_label:"default" ~uuid:"123" ~tags:[]
    ~endpoints:["bugtool"] ~processors:[] ~filters:[] ~service_name:"xapi" ;
  main ()

let start_test_span () =
  set ~uuid:"123" ~enabled:true () ;
  let tracer = get_tracer ~name:"test_tracer" in
  let span = Tracer.start ~tracer ~name:"test_task" ~parent:None () in
  set ~uuid:"123" ~enabled:false () ;
  span

let start_test_trace () =
  set ~uuid:"123" ~enabled:true () ;
  let tracer = get_tracer ~name:"test_tracer" in
  let root =
    Tracer.start ~tracer ~name:"test_task" ~parent:None ()
    |> Result.value ~default:None
  in
  let span1 =
    Tracer.start ~tracer ~name:"test_task" ~parent:root ()
    |> Result.value ~default:None
  in
  let span2 =
    Tracer.start ~tracer ~name:"test_task" ~parent:span1 ()
    |> Result.value ~default:None
  in
  let span3 =
    Tracer.start ~tracer ~name:"test_task" ~parent:root ()
    |> Result.value ~default:None
  in
  let span4 =
    Tracer.start ~tracer ~name:"test_task" ~parent:span3 ()
    |> Result.value ~default:None
  in
  let spans = [span2; span1; span3; span4; root] in
  set ~uuid:"123" ~enabled:false () ;
  spans

let test_hashtbl_leaks () =
  let span = start_test_span () in
  match span with
  | Ok x ->
      Alcotest.(check bool)
        "Spans are collected in hashtable"
        (Tracer.span_hashtbl_is_empty ())
        false ;
      Export.set_export_interval 0.2 ;
      let _ = Tracer.finish x in
      Unix.sleepf 0.2 ;
      (* Wait for export to clear hashtbl *)
      Alcotest.(check bool)
        "Span export clears hashtable"
        (Tracer.span_hashtbl_is_empty ())
        true
  | Error e ->
      Alcotest.failf "Span start failed with %s" (Printexc.to_string e)

let raise_exn () = raise (Failure "Test exception message")

let test_b () = raise_exn () + 1 (* non-tail to ensure stack entry created *)

let test_a () = test_b () + 1

let test_all_spans_finish () =
  let trace_spans = start_test_trace () in
  let active_spans, _ = Spans.dump () in
  let _ = List.map (fun span -> Tracer.finish span) trace_spans in
  let remaining_spans, finished_spans = Spans.dump () in
  let result =
    Hashtbl.fold
      (fun k v acc ->
        Option.fold ~none:0 ~some:List.length (Hashtbl.find_opt finished_spans k)
        = List.length v
        && acc
      )
      active_spans true
  in
  Alcotest.(check bool)
    "All spans that are finished are moved to finished_spans" true result ;
  Alcotest.(check int)
    "traces with no spans are removed from the hashtable" 0
    (Hashtbl.length remaining_spans)

let test_tracing_exn_backtraces () =
  let span = start_test_span () in
  match span with
  | Ok x -> (
    try
      let (_ : int) = test_a () in
      ()
    with e -> (
      let stacktrace = Printexc.get_backtrace () in
      let x = Tracer.finish ~error:(e, stacktrace) x in
      match x with
      | Ok (Some span) ->
          let span_stacktrace = Span.get_tag span "exception.stacktrace" in
          debug "STACKTRACE: %s" span_stacktrace ;
          List.iter
            (fun fun_str ->
              let re = Re.Posix.compile_pat (".*" ^ fun_str ^ ".*") in
              let function_match = Re.execp re span_stacktrace in
              Alcotest.(check bool)
                (Printf.sprintf "function %s found in backtrace" fun_str)
                true function_match
            )
            ["raise_exn"; "test_b"; "test_a"]
      | Ok None ->
          Alcotest.failf "Span finish failed"
      | Error _ ->
          Alcotest.failf "Failed to fetch exception stacktrace"
    )
  )
  | Error e ->
      Alcotest.failf "Span start failed with %s" (Printexc.to_string e)

let test_attribute_validation () =
  let good = ("foo", "bar") in
  let number = ("foo1", "bar") in
  let snake = ("fo_o", "bar") in
  let namespace = ("xs.foo", "bar") in
  let empty_value = ("foo", "") in
  let empty_key = ("", "bar") in
  let capital = ("Foo", "bar") in
  let start_period = (".foo", "bar") in
  let end_period = ("foo.", "bar") in
  let start_underscore = ("_foo", "bar") in
  let end_underscore = ("foo_", "bar") in
  let too_long_key =
    ( "wtfuphowwruztfzmreivxecuvupijqfvevswqvyxrumelhrkgzsqrrjvkqduiabhfdhhxutmrwloiauesoffowntmqmtiicyprozaivvtzencalwdnkkepcolebicdzrrudeehohgzocmplfzlqaqheyjphhbiqvmclghmjofiayhtaxwpwsahobolxubehbvojodqijszfzmawlkivgagvzmprlpiflzjtvskpthyhwrpemobxmqbpucddgxnha"
    , ""
    )
  in
  let accepted_long_key =
    ( "wtfuphowwruztfzmreivxecuvupijqfvevswqvyxrumelhrkgzsqrrjvkqduiabhfdhhxutmrwloiauesoffowntmqmtiicyprozaivvtzencalwdnkkepcolebicdzrrudeehohgzocmplfzlqaqheyjphhbiqvmclghmjofiayhtaxwpwsahobolxubehbvojodqijszfzmawlkivgagvzmprlpiflzjtvskpthyhwrpemobxmqbpucddgxnh"
    , ""
    )
  in
  let too_long_value =
    ( "foo"
    , "rovmlrouovesemihzezcgqrpvvhnkztvghscxrblrzkiazylqldirxovojdsuqcdmivhvjeszvwzozqfcgysdamukadupaagpvjrtyoaioplhmayhxgljubhfdupnqomlhnnhxyrrfnoatrrumwwbeejxndlikvqhwtdluwssyghtzzjwrzjvllvorgiuralqydlaicqhsytddurqektekezxrxzrzkwiogmspeyhjdunccaecypdzgviclszmugvvkpgcxwjsikqzwnnsuwmhswzrdkbxkosfhnkianqqdvlhejvgowutzjxmrjtwtszphylhzmwrfivqcobipavmabvijruriqmrefqopesfzlspmkizylrhjoondgjyulfohmkvmfsbcyqjfpolozfhegidwjzpgthwirxruofzevrzdwkycnssruzoxfijjghcertipbaosxjovjddlgdotavgjxqqpmrvzaravgxzsajkoxfesjunazsyehmczgcbddnayicfmxbsnyizymvhtcuwfdmugagghjmpwlgwomhwzhxonaqcdfgcgtpmxnanisldkznbixumqgxugvcrncfmceiluexjqxuimcfvpvvplrgvrbumpiyymgbqnunydyqmulbhnhlsralxmlivuzulinjpktoxedrwarnewyvisvpjngbznkcdlfqbjvymqkuakszrzctvcylqdzdwfiaybuevzabzxemdkaohackvkfwgbbeeuqfmyabgktenqdbaeawovynfcfedfzpkrrbnzuwxznrtebgdxnnvcjduibqvbywftnumbhrdanpagpouyhgscwvdylyigcmywbpaoobsvodkzqzlzpnrizhyngomwmdbouxvpddjpzglrafalbufsirtxnkcphqzhmmjpvzzulabpnlkiqexmjgwzrsglpfzerkuismwkhgllzemepxtpohszvpduhowdiutivoowrwkkqaaofltfepwcphjuvingxepfyuvcrnvdkuwaixihmdovqnwvmkxmtttyxddgnamqgdarqsrkleqvdspdpvzgqasaixsamflgoucjvpszkgjrsfevqgwegsgkdjsfraldgdepvxyxapxgwixleuztbitdtswmwchvswmrqjunfazbjrlrghvqdhrvylenhbptqioaiuvvfwwnkvhjagcxphthjznrlidxhbvtkwhgpusoujrvvhohtdqexcrghgrwydreldwmzkqmbulzmavnqzjpxorptyweecwyrlnbcarnjbztubzmpcvmptzhfaqlmgpipqjsilqrqmyhxynyjzpenjvmhiqkexqlastmipedzdkntbjxgemczfkcuvpivjixrayaxkuucergowsvyizvhmshxfytpwkutfutpstktkukktxgbwlzcpkzttaudgkuctuerteuxlmliijdszuxcmoooywyxmnxyhtlizzlpvibzrosespjubzkutaaiaqprmobsbhmeonsjqobrcnjoeivhmibgbzfhcfyndkhpdcwihzkgksintejdexxxeigyhqgkzhpxalufrxghpstdtfmkpoxzkigjthveqnsvvspygwilemjhbxgbxscikagxgxulxzydwpbbsolrrzwksitbwozcwoorbhjqjyzkgjxoinajeoflhmwuwffgtuwbanukifldswlxyakrpbvpftxhvxxcvumzlxfbfdkhvfkznvwycmwlqqjvapjnvfowzckmrwcigldhxoizjehyyhrtiabukdzujdcvetpnhnuwntcicojyscxdkfquwhpbpkzglppmgmsnukphgjxdsdkltdmutxcpezfjurcscxiwzekfrjymmwufiyerlghbkhkylvgbdrrcozjdfqiksrnueapusftvdcaneovhzqkhlastodaabklqoqfuggfglaxpttnlgoadfuiazdcwcfyhkedatgxkwzsihcfusbrodhkpflgvhjojtryhqdlmnvinwupmqohtmvraxriuhbgjatyvccorpqpzacbbxcjiahowffbsiafdimeyvsfmhyeecofwdkqkasonlzowcrjpgxxcvpvxcnzfukrfcqkclbvyulirbqexekviezidaegrmpqllmsgjjnewoyuxxqlmjoppkyrcxyqmaxpjznjpgxdgjnepaaavvlcagxecclsdjxasvgsqhwqycwuouinynoirptvhnkuctfylplehpsovtjqyjwnhicpitmcrzgejhmphjhrfhhwqsdtqrexnykjihtqlwyclapwexrqfmjimswtmngcuyriyoadgswgvwfzwzzvxnvoveklncgboaynujqkqnjvzodrgszrptfbzpobilzspvzsxkphvoibphpfwqvzfccvtxgajjvblwebjtjfntkquunebusqqqzayutzqrevjzdyskaostzsenwbyhukepxnxpexgpypwkpubmokwyazhxvalwepuqjpqmyqdykmaerkfgvxdaybwpwziyjgokmgnqbaphgfbvjdhcofoiabibzweafehdceokxmkhjqjdxgzbsnwdjrqgqghxmgpzxxodsbzttsiexbkeeghmqllvcqwfueeyvmvxumbmzpihldmkehxiaglhzsrkvuvbdzhotgpmqrczeysvjxtoivvgntjutbvstgwuvmrddlzvfnnukmscjzwscpqiuhdpmnnccktzzdpmrplyfrkoshtpfhzhfioafoqaughfqyznirdywurhrhnxxwtzwlqxyuafbotouliofepelgaemxxrhxowvfklklontnauimurflwcxscfskdubskjlokcvwcamgxhvabwumcbupfexfntlpadlwgjziflaaqjxeithotyzfwmmsmtrwgbpjbpqvxqnmozlftpeflpnychwrmcbiylhqnoceegbiahtgwvevzerafwnvxtfvzkpsjthdnidxyzsnrvgvbyozvzedgsnwgwcmetawlhjjikkulvbxlufahbzeuajmsvftradgptureuacobohuyuqqaudrghgacegwdbsvoosxkfywtaacdiicymfockrwhsooggusxxtkzeukkimssqdzrkivfqhfztlsfvmnvcbctzbwortijwwhisdgpgkxfwkshwxknwzvdcwadtlwngintslpcrfnvkoohamexlvhboasgjhcpemqgsnapeseywkhkmugfisvhdscadwovroprjgqhvnublqcbbrmdrlvplprctjvuicaqlmvqgrepxrtvblixibpzweybyyemrnpbgacqtsskxqeyzjicqpokzqnbxrnrrimchavfkbozxbmswyyborgsbdpahbeajqcgpreauwtimkylszuvmuuqyzbymzmcxwiqqtqsidgwrbepeecpbdlpniwmjourrzwzpsxypraibpmapxouwixlphhupqprllkdejsbybdqhgcjrgmmoicepibcyjtwgrgopewzpeeavxmrqphosxxfwadelhwymryugjjizggvfvukgxgfxwizdqupuhmrwnpqzyydboljgckrublpbhsimgwkbenimnflxvloqcuzmnympymtrczdgzjvetqqtlspykkpmjwxvmvsgerxalbapegpjyjnrsqujbnuwwnuvkubpihotjaeuvnuithgwzrpsmxcgqiazhaphpyriqjtryilsoqtlqzyegzrywpyllpgwmvysimnpwigdwvnkfutnsehvbbxwqptoxsnicbqqxbwocfeqwfvhefdlpfoswbrmjzhcwkdquljpelmtwyykhrjkicipyueyijicgcggftdkzqsuqpfpfonyrilunoxxzlmzhgatosrbldnsqbypdcakpzmzsugpqnfyndtrfewgxduwuxkczoewmjkeclmthlcjcxrsuyeszgwtllhccgwevuccvwvxitbsrhbmkwnuakedtqnofhviyrsjjuesybzgzlwlwdloobilntcrnltcxfjiccqgbxqzxixkqnvderfeetpwbzugooalmddmwjemzkfya"
    )
  in
  let accepted_long_value =
    ( "foo"
    , "hghlalxthfhzmxetbkycuksigldfmsnshkqobezlgltebpnijbryhicwxqhkupaxiskdfpviutbsxfoshoputqdjxghkqxlcgdmiyykdpxhzllexqnoxdpxposvmrzcwrptjhxrldcxbjzriwyvjyfkqagvnnnboqbdsblhtcxrzfabksheiynqkzaaowlktaollfgiwfgchtydbnvdzvzlbxythfhlsmgortqgdchlxfstnausluyboqsijjkoprnjbwdkkjhaaobobpfczuakhhxodjhvxazzighjmyhsnixdpinyikprohgnxfoboggenkyfbtspaznvptkgecuboddzpswomulnydfktzzxabaprupovgssoaitddxlogumiqspbuglowpkbqajjmraysnftlfvgfwhbuknhiqvvcvmxgrnviboqdaherfkgmegxcgjhfbbcaghpgppyvflyqdlgxbssosnwmhptwqpqpxkztxeagirzxqacieztcsrqspekkgefvpyymtqaxexvsdrayfrsgsadpvqvqbsfloabkuluraaeuwaqwbazzmvxfgrhepqttocslkwcrlrvbvecdfdloqvnqijonxnkjrdrrhylajiotwtrjhckislyaqdeexpvklqluawobdmmxapwzvayfmmcjcsztypnneeauuyfwhkpkxgyylwrsolurrxytogsakpvqlqvfrbsrcqgewjgbsattxffxbrdoyillsayfthcskavtbnxxuidbpzqksjkrhenzylpxwxhvdwisbadhgmmogkqhrkuivsgxvwpetvviahczvlexzsabtittcqsrqcdwldcdxaufexujbwutzydixnzdtpqumpppwzmkntfbmqpqtyhpjlclnpvbuflxwebfmsjnvkzeequxxjsrtoqfsvutiladhttufmykkitctqeliunekevntumbyitjxcxxuaoqsrnupwihhlulkzpkbdvdcfddebnvkmzpigthdhkxrouqbypxwcibjhogjbfsijhlbjprcxeeyxsdwottbqvbjvlvexgofqubhlforcsqbqkqxklrhcdlnoovsvfxrhrlwqbwdvxnkphzuqwfnvrlyamnbqwdiorhdmvdfpekuikgqxktdtgybqpzvqycbivhhllimcolxenvwiltzzahxvvvgzxxrrknnlvdyjewwugvpnpzujlujpiofbkaohjqsbndcdhbbvqzzwdlvmihejdwnztxvyrzurvrdwjixqotvkmtxladbhmfznvxebbfsaqxivdtktnbtqehyvmavpfzvybascwpaahmuskwnnkijruhqhumhoypriqhyrvjbwmwornucitwskdpexlwcbqlxttaulvqrudkzzvhwcarbeoarvaktztgnbppgzwjqakdxrzeppvabmqmqxhvpsdrzqvqsojghhxmlzcjweghhbouvuihjxhzpwscppmfptpaokfnaydofeedxxbtutjgxzntigmsrinkulhyskchtkdnkqsnoteyetkszmxwmtxudwjhyjdiznghectneaqsbaxbeybhayknvekvwqhbaqrvpcequagvwacrwyvmebsdslrsgjipsvvzqidmjpfffphyvqnpthhpgazqmbsrokrquytbyfaeqkqbkjdvzfhrdqorkqmarzmmzrvtgmuhlvxvkyvpgplwoimnpifwnmokwxvvgpmpumbiecdfczfhnhmvqsqsthnpwfyemzgwvuwqgcmzuygoouzfqzgdguwcrvzteqctvjwgszkmkgfzwjblgcjdwkqqaagiyqxfobchkngoevugothuqudcsurtdehxuwzmeghsqjrblokgllwnavjzvhykspjmioeevheajwnkrlfpjcpahedyyjugbqlyfbetaivnuxvlenrgjryhixsqmmvvuuoapqifqkrwruevdvncpeemsxqlueljntbwrxbbmmslpihfwfoqtdcucddxzkghvaxtyjtmtznffakufvgxasznqoqmaknjzzetyszgboysaazgwfsyiftwprtfugvewjzxiobvjubclobpsbsvegrykrjhbwasopmetxpptalmlbthebzvzpagqzqveetegfakofrlgpoquyyoekrogbontukuxftzgkohvqyyyhpqwinfqphyzahtdvadywkvsrzpystskcyvdytlrcvuaasbziqdscrbdfsukpyoqzqwevpfxtgpleqikcccvnfjddvwiilmpovmxzpwjrcjtagzmnytjolpoawbxpbpnzbebafirnlnaxwvgnzsgkzdtrvqnnvxrznrzegudpfbmmoxldhlpkfynkshijwuofmhzpueeoljuwgphaogfiuwycreevkhizgjxsfafkuohombprrgvfzsfiwpmbblqiqucowjbusbgaptuyoyimsrgxxorywyqsqlarphyybblakeutcebvmuifkceqvotrpnwmciftjutpvhporjsdixwcstipodmuspxeudjrosatpkwwydrbpcamlibqiscmefmyntozdgloojrsonpvtfudjfrmomtaivrazjvlyhmadnkdyynzhhisgmpyoqsbxhpnqdyrmgiwzprxezpvoojhjgwkkllfjgbwblkqjtayqnlfuumvvuyuwbuhukwnmmzpipgazfojegahbzixbriromwjjtopjfieiaakttkeuquagmviwltmvlxmerkantqhtpbxjzezcwtxbscoezpaqrssqtrlqknmkeqmqqszwyiiibxfvrdtzwopdbaxgscpsgorkgpuddehaiwotztwrcwpkbzqnighsywxccvnteeifowxzzpghbwpcgvocvyiqzfdkueqogtdnzdzxglrkfgrixpwqjqyswppkewcnafkkzqrodcfaojhqcdupxqbhwdncetgmagztxusqqpflrwxbxrnghbxwtrjvbfwnvssunhngqlxgmhqrwjrudissoibipvvqotiqyieaxzbyjuvmhvnpsuvhpcacssxxaewecwewqtkfzpqurhpgtvmcrltsmqvwokbeikuvbtzntinxrqaseheeqyzxfogexlfqlndrnucyxegblmqtubuzocgdhgndpctndrytgdhakadfamgsgpcxzijtusvdhudnrjtnsxqjkeqsplkjvuwiqngzqzkkbsyndsbdjvrwqkimzxttzdtvglofangtuggacqiqxawbwakxdtzgoabbjvsnofhxqiwuyczpypthsgehottjrhuyolsbnojkcevbrhaavupxyzjjecwqncpwhsvfyptqnytrbdqbxcrvtgfrhwvuonhxxqtwnhmiiaqzjavyggphohawxxbzcpqshdorkbkvdrmkynyrvzlfstnihnvszisttigojevrdhtrlcxmouityrhmiuclynnznrscvkantiaggbaxvegjvgnvspnziffsrjzpbtgvsoxuroezxdjksjawkxamrkmorjaoluvdlijkgqswqonutpcigjllvlxwthpwmpagvhwwpplwfmrclldneoyerymcvobopeyzsmmfptoxmqmmfllqqkdaiigctuwymsfrcrfeyarxtdkgyxjokvynatcvxylfozaqjtryeltrkzuogaoaxvzoqxibdsgzjgaqccfivjtgxrxtecqfxyckauilimzcxccudxmpffshmmivumpcrizlravdcafbiqgniydncfynslmdmtamxlqdzqupoohpdnrgemepisppgwkpgkycjkvdaojvempoeppletwnxssbwfyplhhuztmsktmvkwlcdfszwhmjgylqdisbtuhtzxxedjypgwhpkqvyqbchvxnbldqeukjkvtcnsbxpwuogkdyqwnqvmpwwnruvodbfnyhfqwlosdhzvuvqyyjhwnobfxhrccyfonyfwqubxruamisovtmfnrzsefblzoajozyblxuzcklsnirhvlunobsoaqxhyusnxayyzevgzwndrzsuyxhhccoznketmdxnxdunymekiklbpjxcnko"
    )
  in
  Alcotest.(check bool)
    "Good key, value pair" true
    (Tracing.validate_attribute good) ;
  Alcotest.(check bool)
    "Good key, value pair with number" true
    (Tracing.validate_attribute number) ;
  Alcotest.(check bool)
    "Good key, value pair with snake case" true
    (Tracing.validate_attribute snake) ;
  Alcotest.(check bool)
    "Good key, value pair with namespace" true
    (Tracing.validate_attribute namespace) ;
  Alcotest.(check bool)
    "Good key, value pair with empty value" true
    (Tracing.validate_attribute empty_value) ;
  Alcotest.(check bool)
    "Good key, value pair with long but accepted key name" true
    (Tracing.validate_attribute accepted_long_key) ;
  Alcotest.(check bool)
    "Good key, value pair with long but accepted value" true
    (Tracing.validate_attribute accepted_long_value) ;
  Alcotest.(check bool)
    "Bad key, value pair with empty key" false
    (Tracing.validate_attribute empty_key) ;
  Alcotest.(check bool)
    "Bad key, value pair with capital key" false
    (Tracing.validate_attribute capital) ;
  Alcotest.(check bool)
    "Bad key, value pair with starting period" false
    (Tracing.validate_attribute start_period) ;
  Alcotest.(check bool)
    "Bad key, value pair with ending period" false
    (Tracing.validate_attribute end_period) ;
  Alcotest.(check bool)
    "Bad key, value pair with starting underscore" false
    (Tracing.validate_attribute start_underscore) ;
  Alcotest.(check bool)
    "Bad key, value pair ending underscore" false
    (Tracing.validate_attribute end_underscore) ;
  Alcotest.(check bool)
    "Bad key, value pair with too long key name" false
    (Tracing.validate_attribute too_long_key) ;
  Alcotest.(check bool)
    "Bad key, value pair with too long value" false
    (Tracing.validate_attribute too_long_value)

let test =
  [
    ("test_hashtbl_leaks", `Quick, test_hashtbl_leaks)
  ; ("test_tracing_exn_backtraces", `Quick, test_tracing_exn_backtraces)
  ; ("test_all_spans_finish", `Quick, test_all_spans_finish)
  ; ("test_attribute_validation", `Quick, test_attribute_validation)
  ]

let () = Alcotest.run "Tracing" [("Tracing lifetime", test)]
