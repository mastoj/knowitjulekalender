open System.Collections.Generic

module Queue =
    let enqueue<'T> item (q:Queue<'T>) = 
        q.Enqueue(item)
        q
    
    let dequeue<'T> (q:Queue<'T>) = 
        if q.Count > 0 
        then
            let item = q.Dequeue()
            Some (item,q)
        else
            None
        
    let peek<'T> (q:Queue<'T>) = 
        q.Peek()

let checkPattern map targetMap = 
    targetMap 
    |> Map.fold (fun s k v -> s && map |> Map.containsKey k && map |> Map.find k |> ((<=) v)) true

let incrMapItem key map = 
    match map |> Map.tryFind key with
    | None -> map |> Map.add key 1
    | Some x -> map |> Map.add key (x+1)
let decrMapItem key map = 
    match map |> Map.find key with
    | x -> map |> Map.add key (x-1)

let findSubstring (text:string) substr =
    let mapPattern = 
        substr 
        |> Seq.groupBy (fun c -> c) 
        |> Seq.map (fun (k,v) -> k,(v |> Seq.length))
        |> Map.ofSeq
    let rec generate currentStart currentStop remText best map (queue:Queue<char>) =
        match remText with
        | c::rest ->
            let map' = map |> incrMapItem c
            let queue' = queue |> Queue.enqueue c
            if checkPattern map' mapPattern
            then 
                let best' = 
                    match best with
                    | Some (f,l) -> if currentStop-currentStart < l-f then Some (currentStart,currentStop) else best
                    | None -> Some (currentStart,currentStop) 
                breakDown (currentStart+1) (currentStop+1) rest best' map' queue'
            else
                generate currentStart (currentStop+1) rest best map' queue'
        | [] -> 
            match best with
            | Some (start,stop) -> 
                Some (text.Substring(start,(stop-start)))
            | None -> None
    and breakDown currentStart currentStop remText best map (queue:Queue<char>) = 
        match queue |> Queue.dequeue with
        | None -> generate currentStart currentStop remText best map queue
        | Some (c,queue') ->
            let map' = map |> decrMapItem c
            if checkPattern map' mapPattern
            then 
                let best' = 
                    match best with
                    | Some (f,l) -> if currentStop-currentStart < l-f then Some (currentStart,currentStop) else best
                    | None -> Some (currentStart,currentStop) 
                breakDown (currentStart+1) currentStop remText best' map' queue'
            else
                generate currentStart currentStop remText best map' queue'
    generate 0 0 (text |> Seq.toList) None Map.empty (new Queue<char>())
    
let text = """FJKAUNOJDCUTCRHBYDLXKEODVBWTYPTSHASQQFCPRMLDXIJMYPVOHBDUGSMBLMVUMMZYHULSUIZIMZTICQORLNTOVKVAMQTKHVRIFMNTSLYGHEHFAHWWATLYAPEXTHEPKJUGDVWUDDPRQLUZMSZOJPSIKAIHLTONYXAULECXXKWFQOIKELWOHRVRUCXIAASKHMWTMAJEWGEESLWRTQKVHRRCDYXNT
LDSUPXMQTQDFAQAPYBGXPOLOCLFQNGNKPKOBHZWHRXAWAWJKMTJSLDLNHMUGVVOPSAMRUJEYUOBPFNEHPZZCLPNZKWMTCXERPZRFKSXVEZTYCXFRHRGEITWHRRYPWSVAYBUHCERJXDCYAVICPTNBGIODLYLMEYLISEYNXNMCDPJJRCTLYNFMJZQNCLAGHUDVLYIGASGXSZYPZKLAWQUDVNTWGFFY
FFSMQWUNUPZRJMTHACFELGHDZEJWFDWVPYOZEVEJKQWHQAHOCIYWGVLPSHFESCGEUCJGYLGDWPIWIDWZZXRUFXERABQJOXZALQOCSAYBRHXQQGUDADYSORTYZQPWGMBLNAQOFODSNXSZFURUNPMZGHTAJUJROIGMRKIZHSFUSKIZJJTLGOEEPBMIXISDHOAIFNFEKKSLEXSJLSGLCYYFEQBKIZZTQQ
XBQZAPXAAIFQEIXELQEZGFEPCKFPGXULLAHXTSRXDEMKFKABUTAABSLNQBNMXNEPODPGAORYJXCHCGKECLJVRBPRLHORREEIZOBSHDSCETTTNFTSMQPQIJBLKNZDMXOTRBNMTKHHCZQQMSLOAXJQKRHDGZVGITHYGVDXRTVBJEAHYBYRYKJAVXPOKHFFMEPHAGFOOPFNKQAUGYLVPWUJUPCUGGIXGR
AMELUTEPYILBIUOCKKUUBJROQFTXMZRLXBAMHSDTEKRRIKZUFNLGTQAEUINMBPYTWXULQNIIRXHHGQDPENXAJNWXULFBNKBRINUMTRBFWBYVNKNKDFR"""
let pattern = "ABCDA"
findSubstring text pattern |> printfn "Behold: %A"
