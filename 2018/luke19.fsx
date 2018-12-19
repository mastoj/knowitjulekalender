
let input = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. God jul."
let expected = "The quick brown fox jumps over the lazy dog. Godt nytt år."

let computeEditDistance (source:string,target:string) = 
  let height,width = (source.Length, target.Length)
  let grid: int [,]  = Array2D.zeroCreate<int> (height+1) (width+1)

  for h = 0 to height do 
    for w = 0 to width do 
      grid.[h,w] <- 
        match h,w with 
        | h,0  -> h    // case 1 and 2 
        | 0, w -> w
        | h, w -> 
            let s,t = source.[h-1],target.[w-1]
            let substitution = grid.[h-1,w-1]+(if s = t then 0 else 1) 
            let insertion = grid.[h,w-1] + 1 
            let deletion  = grid.[h-1,w] + 1
            min (min insertion deletion) substitution // case 3
  grid.[height,width]

computeEditDistance (input, expected)