type Agent<'T> = MailboxProcessor<'T>

let agent = Agent.Start(fun (inbox:Agent<string>) ->
    let rec loop() =
        async {
            let! msg = inbox.Receive()
            match msg with
            | "" -> 
                printfn "Stopping agent"
            | _ -> 
                printfn "Message recieved: %s" msg
                return! loop() 
        }
    loop() 
)

let post (agent:Agent<'T>) message = agent.Post message
let postAsyncReply (agent:Agent<'T>) messageConstr = agent.PostAndAsyncReply(messageConstr)

"hello world" |> post agent
"" |> post agent
"hello" |> post agent
