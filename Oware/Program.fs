module Oware

type StartingPosition =
    | South
    | North

type GameState = 
    | South's_turn
    | North's_turn
    | Game_Ended_in_a_draw
    | South_won
    | North_won

type Player = {
 houses_number : int*int*int*int*int*int   //Each player has six houses
 captured : int
}
 
type Board = {
 player1: Player
 player2: Player  
 gameState: GameState 
}

let getSeeds n board = failwith "Not implemented"
 let (a,b,c,d,f,e),(q,w,r,t,y,u)= board.player1.houses_number,board.player2.houses_number
 match n with 
    |1-> a
    |2-> b 
    |3-> c
    |4-> d
    |5-> f
    |6-> e
    |7-> q
    |8-> w
    |9-> r
    |10-> t
    |11-> y
    |12-> u
    |_-> failwith "Number is above or below"

let isInCorrectHouse n p=       
    match p with
    |South's_turn ->
       match n with
       |1|2|3|4|5|6 ->true
       |_->false
    |North's_turn ->
         match n with
         |7|8|9|10|11|12 ->true
         |_-> false  
    |_-> failwith "ucabangani"

let turn board=
    match board.player1.captured =24 && board.player2.captured= 24 with
       |true->Game_Ended_in_a_draw
       |_->
        match board.player1.captured >=25 with  
        |true->South_won
        |_->
            match board.player2.captured>=25 with
            |true->North_won
            |_->
                 match board.gameState with
                 |South's_turn->North's_turn
                 |North's_turn->South's_turn
                 |_-> failwith "You"   


let setSelectedHouseToZero n board =                      //Making the house choosen zero as the seeds will be distributed
   let (a,b,c,d,f,e)= board.player1.houses_number
   let (q,w,r,t,y,u)=board.player2.houses_number
   match n with 
    |1->{board with player1= {board.player1 with houses_number=(0,b,c,d,f,e)}}                  //updating the board      
    |2->{board with player1= {board.player1 with houses_number=(a,0,c,d,f,e)}} 
    |3->{board with player1= {board.player1 with houses_number=(a,b,0,d,f,e)}} 
    |4->{board with player1= {board.player1 with houses_number=(a,b,c,0,f,e)}} 
    |5->{board with player1= {board.player1 with houses_number=(a,b,c,d,0,e)}} 
    |6->{board with player1= {board.player1 with houses_number=(a,b,c,d,f,0)}} 
    |7->{board with player2= {board.player2 with houses_number=(0,w,r,t,y,u)}} 
    |8->{board with player2= {board.player2 with houses_number=(q,0,r,t,y,u)}}
    |9->{board with player2= {board.player2 with houses_number=(q,w,0,t,y,u)}}
    |10->{board with player2= {board.player2 with houses_number=(q,w,r,0,y,u)}}
    |11->{board with player2= {board.player2 with houses_number=(q,w,r,t,0,u)}}
    |12->{board with player2= {board.player2 with houses_number=(q,w,r,t,y,0)}}
    |_->failwith "You have reached the limit!"


//Method which adds one to the specified house and returns a updated board

let addOneToHouse n (a,b,c,d,f,e,q,w,r,t,y,u)=
    match n with
    |1-> ((a+1),b,c,d,f,e,q,w,r,t,y,u)
    |2-> (a,(b+1),c,d,f,e,q,w,r,t,y,u) 
    |3-> (a,b,(c+1),d,f,e,q,w,r,t,y,u)
    |4-> (a,b,c,(d+1),f,e,q,w,r,t,y,u)
    |5-> (a,b,c,d,(f+1),e,q,w,r,t,y,u)
    |6-> (a,b,c,d,f,(e+1),q,w,r,t,y,u)
    |7-> (a,b,c,d,f,e,(q+1),w,r,t,y,u)
    |8-> (a,b,c,d,f,e,q,(w+1),r,t,y,u)
    |9-> (a,b,c,d,f,e,q,w,(r+1),t,y,u)
    |10-> (a,b,c,d,f,e,q,w,r,(t+1),y,u)
    |11-> (a,b,c,d,f,e,q,w,r,t,(y+1),u)
    |12-> (a,b,c,d,f,e,q,w,r,t,y,(u+1))
    |_-> failwith "Not implemented"
 
let house n board=
    let seep =getSeeds n board
    match (seep+n)>12 with
    |true->(seep+n)-12
    |_-> seep+n
    
let houseToTakeSeedsFrom n =
 match n with
  |1|2|3|4|5|6 -> North's_turn
  |_->South's_turn

let isPlayerHousesEmpty board = 
    match board.gameState with
    |North's_turn->
        let(a,b,c,d,e,f)=board.player1.houses_number
        match (a,b,c,d,e,f) with
        |(0,0,0,0,0,0)->true
        |_->false
     |South's_turn->
        let(a,b,c,d,e,f)=board.player2.houses_number
        match (a,b,c,d,e,f) with
        |(0,0,0,0,0,0)->true
        |_->false
    |_->false

let scoreUpdate board  score =
 
 match board.gameState with
  |South's_turn ->
                  let v = {board.player1 with  captured= (score + board.player1.captured) } // updating South's score
                  {board with player1=v}       // updating South's board
  |North's_turn ->
                  let w = {board.player2 with  captured= (score +  board.player2.captured)} // updating North's score
                  {board with player2=w}       // updating North's board
  |_-> board

let toPlayer board=
    match board.gameState with
     |South's_turn ->North's_turn            
     |North's_turn -> South's_turn
     |_->failwith "Let's fail with something!"
                  
let correctHouse board latestHouseNum  =
    let initialBoard=board
    let rec ToThinkAbout houseFrom b acc =
        match (toPlayer b) = (houseToTakeSeedsFrom houseFrom) with
        |true -> scoreUpdate b acc
        |false ->
            match (getSeeds houseFrom b) with
            | 3 -> ToThinkAbout (houseFrom - 1) (setSelectedHouseToZero houseFrom b) (acc + 3)
            | 2 -> ToThinkAbout (houseFrom - 1) (setSelectedHouseToZero houseFrom b) (acc + 2)
            | _ -> 
                match isPlayerHousesEmpty b with
                |true->scoreUpdate b 0
                |_->
                   scoreUpdate b acc
    ToThinkAbout latestHouseNum board 0
    
let useHouse n board = 
  let seedCount = getSeeds n board
  let rig=house n board
  match isInCorrectHouse n board.gameState with
  |false-> board
  |true-> 
  match isPlayerHousesEmpty board with
  |true->board
  |_->
  match seedCount with
  |0-> board
  |_-> 
  let (a,b,c,d,f,e)= (setSelectedHouseToZero n board).player1.houses_number
  let (q,w,r,t,y,u)= (setSelectedHouseToZero n board).player2.houses_number
  let newBoard= (a,b,c,d,f,e,q,w,r,t,y,u)

        //This method does the actual moving/sowing (it is going to distributes the seeds collected) 
  let rec move n seed newhouse bs=  
   let p=match n with              //Binding the results of the match to p so that we can use it as a house counter
           |13->1                 //This is where we start adding from the first house again as the game has 12 houses/ creating a loop between 1-12
           |_-> n
   match seed<> 0 with
           |false->                         //Base case of the recursive function: return the new updated board       
                   let (a,b,c,d,f,e,q,w,r,t,y,u)=newhouse 
                   let southplayer= {houses_number=(a,b,c,d,f,e);captured=board.player1.captured}
                   let northplayer= {houses_number=(q,w,r,t,y,u);captured=board.player2.captured}
                   let r= {board with player1=southplayer;player2=northplayer;gameState=board.gameState}
                   correctHouse r rig
           |true->  
              match p=bs with 
              |true->move (p+1) seed newhouse bs    //
              |_->move (p+1) (seed-1) (addOneToHouse p newhouse) bs

  let nB= move (n+1) seedCount newBoard n 
  {nB with gameState= turn nB}

let start position = failwith "Not implemented"
  let southplayer= {houses_number=(4,4,4,4,4,4);captured=0}
  let northplayer= {houses_number=(4,4,4,4,4,4);captured=0}
  let r=match position with 
         |South->South's_turn
         |North->North's_turn
  {Board.player1=southplayer;Board.player2=northplayer;gameState= r}

let score board = board.player1.captured,board.player2.captured

let gameState board = 
    match board.gameState with 
    |South's_turn-> "South's turn"
    |North's_turn->"North's turn"
    |Game_Ended_in_a_draw-> "Game ended in a draw"
    |South_won-> "South won"
    |North_won-> "North won"

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
module Oware

type StartingPosition =
    | South
    | North

type GameState = 
    | South's_turn
    | North's_turn
    | Game_Ended_in_a_draw
    | South_won
    | North_won

type Player = {
 houses_number : int*int*int*int*int*int   //Each player has six houses
 captured : int
}
 
type Board = {
 player1: Player
 player2: Player  
 gameState: GameState 
}

let getSeeds n board = failwith "Not implemented"
 let (a,b,c,d,f,e),(q,w,r,t,y,u)= board.player1.houses_number,board.player2.houses_number
 match n with 
    |1-> a
    |2-> b 
    |3-> c
    |4-> d
    |5-> f
    |6-> e
    |7-> q
    |8-> w
    |9-> r
    |10-> t
    |11-> y
    |12-> u
    |_-> failwith "Number is above or below"

let isInCorrectHouse n p=       
    match p with
    |South's_turn ->
       match n with
       |1|2|3|4|5|6 ->true
       |_->false
    |North's_turn ->
         match n with
         |7|8|9|10|11|12 ->true
         |_-> false  
    |_-> failwith "ucabangani"

let turn board=
    match board.player1.captured =24 && board.player2.captured= 24 with
       |true->Game_Ended_in_a_draw
       |_->
        match board.player1.captured >=25 with  
        |true->South_won
        |_->
            match board.player2.captured>=25 with
            |true->North_won
            |_->
                 match board.gameState with
                 |South's_turn->North's_turn
                 |North's_turn->South's_turn
                 |_-> failwith "You"   


let setSelectedHouseToZero n board =                      //Making the house choosen zero as the seeds will be distributed
   let (a,b,c,d,f,e)= board.player1.houses_number
   let (q,w,r,t,y,u)=board.player2.houses_number
   match n with 
    |1->{board with player1= {board.player1 with houses_number=(0,b,c,d,f,e)}}                  //updating the board      
    |2->{board with player1= {board.player1 with houses_number=(a,0,c,d,f,e)}} 
    |3->{board with player1= {board.player1 with houses_number=(a,b,0,d,f,e)}} 
    |4->{board with player1= {board.player1 with houses_number=(a,b,c,0,f,e)}} 
    |5->{board with player1= {board.player1 with houses_number=(a,b,c,d,0,e)}} 
    |6->{board with player1= {board.player1 with houses_number=(a,b,c,d,f,0)}} 
    |7->{board with player2= {board.player2 with houses_number=(0,w,r,t,y,u)}} 
    |8->{board with player2= {board.player2 with houses_number=(q,0,r,t,y,u)}}
    |9->{board with player2= {board.player2 with houses_number=(q,w,0,t,y,u)}}
    |10->{board with player2= {board.player2 with houses_number=(q,w,r,0,y,u)}}
    |11->{board with player2= {board.player2 with houses_number=(q,w,r,t,0,u)}}
    |12->{board with player2= {board.player2 with houses_number=(q,w,r,t,y,0)}}
    |_->failwith "You have reached the limit!"


//Method which adds one to the specified house and returns a updated board

let addOneToHouse n (a,b,c,d,f,e,q,w,r,t,y,u)=
    match n with
    |1-> ((a+1),b,c,d,f,e,q,w,r,t,y,u)
    |2-> (a,(b+1),c,d,f,e,q,w,r,t,y,u) 
    |3-> (a,b,(c+1),d,f,e,q,w,r,t,y,u)
    |4-> (a,b,c,(d+1),f,e,q,w,r,t,y,u)
    |5-> (a,b,c,d,(f+1),e,q,w,r,t,y,u)
    |6-> (a,b,c,d,f,(e+1),q,w,r,t,y,u)
    |7-> (a,b,c,d,f,e,(q+1),w,r,t,y,u)
    |8-> (a,b,c,d,f,e,q,(w+1),r,t,y,u)
    |9-> (a,b,c,d,f,e,q,w,(r+1),t,y,u)
    |10-> (a,b,c,d,f,e,q,w,r,(t+1),y,u)
    |11-> (a,b,c,d,f,e,q,w,r,t,(y+1),u)
    |12-> (a,b,c,d,f,e,q,w,r,t,y,(u+1))
    |_-> failwith "Not implemented"
 
let house n board=
    let seep =getSeeds n board
    match (seep+n)>12 with
    |true->(seep+n)-12
    |_-> seep+n
    
let houseToTakeSeedsFrom n =
 match n with
  |1|2|3|4|5|6 -> North's_turn
  |_->South's_turn

let isPlayerHousesEmpty board = 
    match board.gameState with
    |North's_turn->
        let(a,b,c,d,e,f)=board.player1.houses_number
        match (a,b,c,d,e,f) with
        |(0,0,0,0,0,0)->true
        |_->false
     |South's_turn->
        let(a,b,c,d,e,f)=board.player2.houses_number
        match (a,b,c,d,e,f) with
        |(0,0,0,0,0,0)->true
        |_->false
    |_->false

let scoreUpdate board  score =
 
 match board.gameState with
  |South's_turn ->
                  let v = {board.player1 with  captured= (score + board.player1.captured) } // updating South's score
                  {board with player1=v}       // updating South's board
  |North's_turn ->
                  let w = {board.player2 with  captured= (score +  board.player2.captured)} // updating North's score
                  {board with player2=w}       // updating North's board
  |_-> board

let toPlayer board=
    match board.gameState with
     |South's_turn ->North's_turn            
     |North's_turn -> South's_turn
     |_->failwith "Let's fail with something!"
                  
let correctHouse board latestHouseNum  =
    let initialBoard=board
    let rec ToThinkAbout houseFrom b acc =
        match (toPlayer b) = (houseToTakeSeedsFrom houseFrom) with
        |true -> scoreUpdate b acc
        |false ->
            match (getSeeds houseFrom b) with
            | 3 -> ToThinkAbout (houseFrom - 1) (setSelectedHouseToZero houseFrom b) (acc + 3)
            | 2 -> ToThinkAbout (houseFrom - 1) (setSelectedHouseToZero houseFrom b) (acc + 2)
            | _ -> 
                match isPlayerHousesEmpty b with
                |true->scoreUpdate b 0
                |_->
                   scoreUpdate b acc
    ToThinkAbout latestHouseNum board 0
    
let useHouse n board = 
  let seedCount = getSeeds n board
  let rig=house n board
  match isInCorrectHouse n board.gameState with
  |false-> board
  |true-> 
  match isPlayerHousesEmpty board with
  |true->board
  |_->
  match seedCount with
  |0-> board
  |_-> 
  let (a,b,c,d,f,e)= (setSelectedHouseToZero n board).player1.houses_number
  let (q,w,r,t,y,u)= (setSelectedHouseToZero n board).player2.houses_number
  let newBoard= (a,b,c,d,f,e,q,w,r,t,y,u)

        //This method does the actual moving/sowing (it is going to distributes the seeds collected) 
  let rec move n seed newhouse bs=  
   let p=match n with              //Binding the results of the match to p so that we can use it as a house counter
           |13->1                 //This is where we start adding from the first house again as the game has 12 houses/ creating a loop between 1-12
           |_-> n
   match seed<> 0 with
           |false->                         //Base case of the recursive function: return the new updated board       
                   let (a,b,c,d,f,e,q,w,r,t,y,u)=newhouse 
                   let southplayer= {houses_number=(a,b,c,d,f,e);captured=board.player1.captured}
                   let northplayer= {houses_number=(q,w,r,t,y,u);captured=board.player2.captured}
                   let r= {board with player1=southplayer;player2=northplayer;gameState=board.gameState}
                   correctHouse r rig
           |true->  
              match p=bs with 
              |true->move (p+1) seed newhouse bs    //
              |_->move (p+1) (seed-1) (addOneToHouse p newhouse) bs

  let nB= move (n+1) seedCount newBoard n 
  {nB with gameState= turn nB}

let start position = failwith "Not implemented"
  let southplayer= {houses_number=(4,4,4,4,4,4);captured=0}
  let northplayer= {houses_number=(4,4,4,4,4,4);captured=0}
  let r=match position with 
         |South->South's_turn
         |North->North's_turn
  {Board.player1=southplayer;Board.player2=northplayer;gameState= r}

let score board = board.player1.captured,board.player2.captured

let gameState board = 
    match board.gameState with 
    |South's_turn-> "South's turn"
    |North's_turn->"North's turn"
    |Game_Ended_in_a_draw-> "Game ended in a draw"
    |South_won-> "South won"
    |North_won-> "North won"

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
module Oware

type StartingPosition =
    | South
    | North

type GameState = 
    | South's_turn
    | North's_turn
    | Game_Ended_in_a_draw
    | South_won
    | North_won

type Player = {
 houses_number : int*int*int*int*int*int   //Each player has six houses
 captured : int
}
 
type Board = {
 player1: Player
 player2: Player  
 gameState: GameState 
}

let getSeeds n board = failwith "Not implemented"
 let (a,b,c,d,f,e),(q,w,r,t,y,u)= board.player1.houses_number,board.player2.houses_number
 match n with 
    |1-> a
    |2-> b 
    |3-> c
    |4-> d
    |5-> f
    |6-> e
    |7-> q
    |8-> w
    |9-> r
    |10-> t
    |11-> y
    |12-> u
    |_-> failwith "Number is above or below"

let isInCorrectHouse n p=       
    match p with
    |South's_turn ->
       match n with
       |1|2|3|4|5|6 ->true
       |_->false
    |North's_turn ->
         match n with
         |7|8|9|10|11|12 ->true
         |_-> false  
    |_-> failwith "ucabangani"

let turn board=
    match board.player1.captured =24 && board.player2.captured= 24 with
       |true->Game_Ended_in_a_draw
       |_->
        match board.player1.captured >=25 with  
        |true->South_won
        |_->
            match board.player2.captured>=25 with
            |true->North_won
            |_->
                 match board.gameState with
                 |South's_turn->North's_turn
                 |North's_turn->South's_turn
                 |_-> failwith "You"   


let setSelectedHouseToZero n board =                      //Making the house choosen zero as the seeds will be distributed
   let (a,b,c,d,f,e)= board.player1.houses_number
   let (q,w,r,t,y,u)=board.player2.houses_number
   match n with 
    |1->{board with player1= {board.player1 with houses_number=(0,b,c,d,f,e)}}                  //updating the board      
    |2->{board with player1= {board.player1 with houses_number=(a,0,c,d,f,e)}} 
    |3->{board with player1= {board.player1 with houses_number=(a,b,0,d,f,e)}} 
    |4->{board with player1= {board.player1 with houses_number=(a,b,c,0,f,e)}} 
    |5->{board with player1= {board.player1 with houses_number=(a,b,c,d,0,e)}} 
    |6->{board with player1= {board.player1 with houses_number=(a,b,c,d,f,0)}} 
    |7->{board with player2= {board.player2 with houses_number=(0,w,r,t,y,u)}} 
    |8->{board with player2= {board.player2 with houses_number=(q,0,r,t,y,u)}}
    |9->{board with player2= {board.player2 with houses_number=(q,w,0,t,y,u)}}
    |10->{board with player2= {board.player2 with houses_number=(q,w,r,0,y,u)}}
    |11->{board with player2= {board.player2 with houses_number=(q,w,r,t,0,u)}}
    |12->{board with player2= {board.player2 with houses_number=(q,w,r,t,y,0)}}
    |_->failwith "You have reached the limit!"


//Method which adds one to the specified house and returns a updated board

let addOneToHouse n (a,b,c,d,f,e,q,w,r,t,y,u)=
    match n with
    |1-> ((a+1),b,c,d,f,e,q,w,r,t,y,u)
    |2-> (a,(b+1),c,d,f,e,q,w,r,t,y,u) 
    |3-> (a,b,(c+1),d,f,e,q,w,r,t,y,u)
    |4-> (a,b,c,(d+1),f,e,q,w,r,t,y,u)
    |5-> (a,b,c,d,(f+1),e,q,w,r,t,y,u)
    |6-> (a,b,c,d,f,(e+1),q,w,r,t,y,u)
    |7-> (a,b,c,d,f,e,(q+1),w,r,t,y,u)
    |8-> (a,b,c,d,f,e,q,(w+1),r,t,y,u)
    |9-> (a,b,c,d,f,e,q,w,(r+1),t,y,u)
    |10-> (a,b,c,d,f,e,q,w,r,(t+1),y,u)
    |11-> (a,b,c,d,f,e,q,w,r,t,(y+1),u)
    |12-> (a,b,c,d,f,e,q,w,r,t,y,(u+1))
    |_-> failwith "Not implemented"
 
let house n board=
    let seep =getSeeds n board
    match (seep+n)>12 with
    |true->(seep+n)-12
    |_-> seep+n
    
let houseToTakeSeedsFrom n =
 match n with
  |1|2|3|4|5|6 -> North's_turn
  |_->South's_turn

let isPlayerHousesEmpty board = 
    match board.gameState with
    |North's_turn->
        let(a,b,c,d,e,f)=board.player1.houses_number
        match (a,b,c,d,e,f) with
        |(0,0,0,0,0,0)->true
        |_->false
     |South's_turn->
        let(a,b,c,d,e,f)=board.player2.houses_number
        match (a,b,c,d,e,f) with
        |(0,0,0,0,0,0)->true
        |_->false
    |_->false

let scoreUpdate board  score =
 
 match board.gameState with
  |South's_turn ->
                  let v = {board.player1 with  captured= (score + board.player1.captured) } // updating South's score
                  {board with player1=v}       // updating South's board
  |North's_turn ->
                  let w = {board.player2 with  captured= (score +  board.player2.captured)} // updating North's score
                  {board with player2=w}       // updating North's board
  |_-> board

let toPlayer board=
    match board.gameState with
     |South's_turn ->North's_turn            
     |North's_turn -> South's_turn
     |_->failwith "Let's fail with something!"
                  
let correctHouse board latestHouseNum  =
    let initialBoard=board
    let rec ToThinkAbout houseFrom b acc =
        match (toPlayer b) = (houseToTakeSeedsFrom houseFrom) with
        |true -> scoreUpdate b acc
        |false ->
            match (getSeeds houseFrom b) with
            | 3 -> ToThinkAbout (houseFrom - 1) (setSelectedHouseToZero houseFrom b) (acc + 3)
            | 2 -> ToThinkAbout (houseFrom - 1) (setSelectedHouseToZero houseFrom b) (acc + 2)
            | _ -> 
                match isPlayerHousesEmpty b with
                |true->scoreUpdate b 0
                |_->
                   scoreUpdate b acc
    ToThinkAbout latestHouseNum board 0
    
let useHouse n board = 
  let seedCount = getSeeds n board
  let rig=house n board
  match isInCorrectHouse n board.gameState with
  |false-> board
  |true-> 
  match isPlayerHousesEmpty board with
  |true->board
  |_->
  match seedCount with
  |0-> board
  |_-> 
  let (a,b,c,d,f,e)= (setSelectedHouseToZero n board).player1.houses_number
  let (q,w,r,t,y,u)= (setSelectedHouseToZero n board).player2.houses_number
  let newBoard= (a,b,c,d,f,e,q,w,r,t,y,u)

        //This method does the actual moving/sowing (it is going to distributes the seeds collected) 
  let rec move n seed newhouse bs=  
   let p=match n with              //Binding the results of the match to p so that we can use it as a house counter
           |13->1                 //This is where we start adding from the first house again as the game has 12 houses/ creating a loop between 1-12
           |_-> n
   match seed<> 0 with
           |false->                         //Base case of the recursive function: return the new updated board       
                   let (a,b,c,d,f,e,q,w,r,t,y,u)=newhouse 
                   let southplayer= {houses_number=(a,b,c,d,f,e);captured=board.player1.captured}
                   let northplayer= {houses_number=(q,w,r,t,y,u);captured=board.player2.captured}
                   let r= {board with player1=southplayer;player2=northplayer;gameState=board.gameState}
                   correctHouse r rig
           |true->  
              match p=bs with 
              |true->move (p+1) seed newhouse bs    //
              |_->move (p+1) (seed-1) (addOneToHouse p newhouse) bs

  let nB= move (n+1) seedCount newBoard n 
  {nB with gameState= turn nB}

let start position = failwith "Not implemented"
  let southplayer= {houses_number=(4,4,4,4,4,4);captured=0}
  let northplayer= {houses_number=(4,4,4,4,4,4);captured=0}
  let r=match position with 
         |South->South's_turn
         |North->North's_turn
  {Board.player1=southplayer;Board.player2=northplayer;gameState= r}

let score board = board.player1.captured,board.player2.captured

let gameState board = 
    match board.gameState with 
    |South's_turn-> "South's turn"
    |North's_turn->"North's turn"
    |Game_Ended_in_a_draw-> "Game ended in a draw"
    |South_won-> "South won"
    |North_won-> "North won"

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
module Oware

type StartingPosition =
    | South
    | North

type GameState = 
    | South's_turn
    | North's_turn
    | Game_Ended_in_a_draw
    | South_won
    | North_won

type Player = {
 houses_number : int*int*int*int*int*int   //Each player has six houses
 captured : int
}
 
type Board = {
 player1: Player
 player2: Player  
 gameState: GameState 
}

let getSeeds n board = failwith "Not implemented"
 let (a,b,c,d,f,e),(q,w,r,t,y,u)= board.player1.houses_number,board.player2.houses_number
 match n with 
    |1-> a
    |2-> b 
    |3-> c
    |4-> d
    |5-> f
    |6-> e
    |7-> q
    |8-> w
    |9-> r
    |10-> t
    |11-> y
    |12-> u
    |_-> failwith "Number is above or below"

let isInCorrectHouse n p=       
    match p with
    |South's_turn ->
       match n with
       |1|2|3|4|5|6 ->true
       |_->false
    |North's_turn ->
         match n with
         |7|8|9|10|11|12 ->true
         |_-> false  
    |_-> failwith "ucabangani"

let turn board=
    match board.player1.captured =24 && board.player2.captured= 24 with
       |true->Game_Ended_in_a_draw
       |_->
        match board.player1.captured >=25 with  
        |true->South_won
        |_->
            match board.player2.captured>=25 with
            |true->North_won
            |_->
                 match board.gameState with
                 |South's_turn->North's_turn
                 |North's_turn->South's_turn
                 |_-> failwith "You"   


let setSelectedHouseToZero n board =                      //Making the house choosen zero as the seeds will be distributed
   let (a,b,c,d,f,e)= board.player1.houses_number
   let (q,w,r,t,y,u)=board.player2.houses_number
   match n with 
    |1->{board with player1= {board.player1 with houses_number=(0,b,c,d,f,e)}}                  //updating the board      
    |2->{board with player1= {board.player1 with houses_number=(a,0,c,d,f,e)}} 
    |3->{board with player1= {board.player1 with houses_number=(a,b,0,d,f,e)}} 
    |4->{board with player1= {board.player1 with houses_number=(a,b,c,0,f,e)}} 
    |5->{board with player1= {board.player1 with houses_number=(a,b,c,d,0,e)}} 
    |6->{board with player1= {board.player1 with houses_number=(a,b,c,d,f,0)}} 
    |7->{board with player2= {board.player2 with houses_number=(0,w,r,t,y,u)}} 
    |8->{board with player2= {board.player2 with houses_number=(q,0,r,t,y,u)}}
    |9->{board with player2= {board.player2 with houses_number=(q,w,0,t,y,u)}}
    |10->{board with player2= {board.player2 with houses_number=(q,w,r,0,y,u)}}
    |11->{board with player2= {board.player2 with houses_number=(q,w,r,t,0,u)}}
    |12->{board with player2= {board.player2 with houses_number=(q,w,r,t,y,0)}}
    |_->failwith "You have reached the limit!"


//Method which adds one to the specified house and returns a updated board

let addOneToHouse n (a,b,c,d,f,e,q,w,r,t,y,u)=
    match n with
    |1-> ((a+1),b,c,d,f,e,q,w,r,t,y,u)
    |2-> (a,(b+1),c,d,f,e,q,w,r,t,y,u) 
    |3-> (a,b,(c+1),d,f,e,q,w,r,t,y,u)
    |4-> (a,b,c,(d+1),f,e,q,w,r,t,y,u)
    |5-> (a,b,c,d,(f+1),e,q,w,r,t,y,u)
    |6-> (a,b,c,d,f,(e+1),q,w,r,t,y,u)
    |7-> (a,b,c,d,f,e,(q+1),w,r,t,y,u)
    |8-> (a,b,c,d,f,e,q,(w+1),r,t,y,u)
    |9-> (a,b,c,d,f,e,q,w,(r+1),t,y,u)
    |10-> (a,b,c,d,f,e,q,w,r,(t+1),y,u)
    |11-> (a,b,c,d,f,e,q,w,r,t,(y+1),u)
    |12-> (a,b,c,d,f,e,q,w,r,t,y,(u+1))
    |_-> failwith "Not implemented"
 
let house n board=
    let seep =getSeeds n board
    match (seep+n)>12 with
    |true->(seep+n)-12
    |_-> seep+n
    
let houseToTakeSeedsFrom n =
 match n with
  |1|2|3|4|5|6 -> North's_turn
  |_->South's_turn

let isPlayerHousesEmpty board = 
    match board.gameState with
    |North's_turn->
        let(a,b,c,d,e,f)=board.player1.houses_number
        match (a,b,c,d,e,f) with
        |(0,0,0,0,0,0)->true
        |_->false
     |South's_turn->
        let(a,b,c,d,e,f)=board.player2.houses_number
        match (a,b,c,d,e,f) with
        |(0,0,0,0,0,0)->true
        |_->false
    |_->false

let scoreUpdate board  score =
 
 match board.gameState with
  |South's_turn ->
                  let v = {board.player1 with  captured= (score + board.player1.captured) } // updating South's score
                  {board with player1=v}       // updating South's board
  |North's_turn ->
                  let w = {board.player2 with  captured= (score +  board.player2.captured)} // updating North's score
                  {board with player2=w}       // updating North's board
  |_-> board

let toPlayer board=
    match board.gameState with
     |South's_turn ->North's_turn            
     |North's_turn -> South's_turn
     |_->failwith "Let's fail with something!"
                  
let correctHouse board latestHouseNum  =
    let initialBoard=board
    let rec ToThinkAbout houseFrom b acc =
        match (toPlayer b) = (houseToTakeSeedsFrom houseFrom) with
        |true -> scoreUpdate b acc
        |false ->
            match (getSeeds houseFrom b) with
            | 3 -> ToThinkAbout (houseFrom - 1) (setSelectedHouseToZero houseFrom b) (acc + 3)
            | 2 -> ToThinkAbout (houseFrom - 1) (setSelectedHouseToZero houseFrom b) (acc + 2)
            | _ -> 
                match isPlayerHousesEmpty b with
                |true->scoreUpdate b 0
                |_->
                   scoreUpdate b acc
    ToThinkAbout latestHouseNum board 0
    
let useHouse n board = 
  let seedCount = getSeeds n board
  let rig=house n board
  match isInCorrectHouse n board.gameState with
  |false-> board
  |true-> 
  match isPlayerHousesEmpty board with
  |true->board
  |_->
  match seedCount with
  |0-> board
  |_-> 
  let (a,b,c,d,f,e)= (setSelectedHouseToZero n board).player1.houses_number
  let (q,w,r,t,y,u)= (setSelectedHouseToZero n board).player2.houses_number
  let newBoard= (a,b,c,d,f,e,q,w,r,t,y,u)

        //This method does the actual moving/sowing (it is going to distributes the seeds collected) 
  let rec move n seed newhouse bs=  
   let p=match n with              //Binding the results of the match to p so that we can use it as a house counter
           |13->1                 //This is where we start adding from the first house again as the game has 12 houses/ creating a loop between 1-12
           |_-> n
   match seed<> 0 with
           |false->                         //Base case of the recursive function: return the new updated board       
                   let (a,b,c,d,f,e,q,w,r,t,y,u)=newhouse 
                   let southplayer= {houses_number=(a,b,c,d,f,e);captured=board.player1.captured}
                   let northplayer= {houses_number=(q,w,r,t,y,u);captured=board.player2.captured}
                   let r= {board with player1=southplayer;player2=northplayer;gameState=board.gameState}
                   correctHouse r rig
           |true->  
              match p=bs with 
              |true->move (p+1) seed newhouse bs    //
              |_->move (p+1) (seed-1) (addOneToHouse p newhouse) bs

  let nB= move (n+1) seedCount newBoard n 
  {nB with gameState= turn nB}

let start position = failwith "Not implemented"
  let southplayer= {houses_number=(4,4,4,4,4,4);captured=0}
  let northplayer= {houses_number=(4,4,4,4,4,4);captured=0}
  let r=match position with 
         |South->South's_turn
         |North->North's_turn
  {Board.player1=southplayer;Board.player2=northplayer;gameState= r}

let score board = board.player1.captured,board.player2.captured

let gameState board = 
    match board.gameState with 
    |South's_turn-> "South's turn"
    |North's_turn->"North's turn"
    |Game_Ended_in_a_draw-> "Game ended in a draw"
    |South_won-> "South won"
    |North_won-> "North won"

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
