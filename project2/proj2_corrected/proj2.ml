open Proj2_types;;

let isAlphabet (value:string) : bool =

        if ((value = "a") || (value = "b") || (value = "c") ||(value = "d") || (value = "e") || (value = "f") || (value = "g") || 
                (value = "h") || (value = "i") || (value = "j") ||(value = "k") || (value = "l") || (value = "m") || (value = "n") || 
                (value = "o") || (value = "p") || (value = "q") ||(value = "r") || (value = "s") || (value = "t") || (value = "u") || 
                (value = "v") || (value = "w") || (value = "x") ||(value = "y") || (value = "z")) then true else false;;

let isBool (s: string) : bool = 
        if ((s = "TRUE") || (s = "FALSE") || (s = "true") || (s = "false")) then true else false;;

let boolOfString (s:string) : bool =
       if ((s = "TRUE")|| (s= "true")) then true else false;;

let buildParseTree (input : string list) : tree = 

        let rec parseS (lst : string list) : (string list * tree) =
                let nonTerminal = "S" in
                match lst with 
                        [] -> ([], TreeNode ("",[]))
                        |h::t -> if(isAlphabet h) then (t,TreeNode (nonTerminal,[TreeNode (h, [])])) else (if (isBool h) then (t, TreeNode (nonTerminal,[TreeNode(h, [])])) else 

                                                                                        let treeOne = TreeNode ("(",[]) in
                                                                                        let treeTwo = TreeNode (")",[]) in
                                                                                        let update  = (parseT t) in
                                                                                        match update with 
                                                                               (hUpdated::tUpdated, subTree) -> (tUpdated, TreeNode (nonTerminal,[treeOne; subTree; treeTwo])))
                                                                                  
        and parseT (lst: string list):(string list * tree) =                                                                                  
                let nonTerminal = "T" in
                match lst with 
                        [] -> ([], TreeNode ("",[]))
                        |h::t ->
                                let childOne = TreeNode (h, []) in
                                let update = (parseS t) in
                                let childTwo = match update with
                                                (updatedList, subTree1) -> subTree1
                                in
                                let newList = match update with
                                                (updatedList, subTree1) -> updatedList
                                in  
                                if (h = "not") then (newList, TreeNode (nonTerminal, [childOne; childTwo])) else 
                                                                                match (parseS newList) with
                                                                           (updatedListTwo, subTree2) -> (updatedListTwo, TreeNode (nonTerminal, [childOne; childTwo; subTree2]))
         in
         let answer = (parseS input) in
        match answer with
                (listAnswer, treeAnswer) -> treeAnswer;; 
                                                
                                
                                                                  
                                                                                         

let rec buildAbstractSyntaxTree (input : tree) : tree = 

        match input with
        TreeNode (root, childList) -> match root with
                                        "S" -> let head = (List.hd childList) in
                                                (match head with
                                                TreeNode (child1ofS, grandChildListofS) -> if (child1ofS = "(") then (buildAbstractSyntaxTree (List.hd (List.tl childList))) 
                                                                                        else TreeNode (child1ofS, []))
                                        |"T" -> match (List.hd childList) with
                                                TreeNode (child1ofT, grandChildListofT)-> if (child1ofT = "not") 
                                                        then TreeNode ("not", [buildAbstractSyntaxTree (List.hd (List.tl childList))])
                                                                else TreeNode (child1ofT, [(buildAbstractSyntaxTree (List.hd (List.tl childList))); 
                                                                                                        (buildAbstractSyntaxTree (List.hd (List.tl (List.tl childList))))]);; 
                                                        
                                                
                                                        
                                       

let scanVariable (input : string list) : string list = 
                
        let rec exists (l: string list) (e: string) : bool = 

                match l with
                []->false
                |h::t -> if e = h then true else (exists (t) (e)) 
        in
        let func (a: string list) (element: string) : string list =
                if ((isAlphabet (element)) && not(exists (a) (element))) then (element :: a) else a
        in
        (List.fold_left func [] input);;  
       

let generateInitialAssignList (varList : string list) : (string * bool) list = 
        
        let func (e: string) : (string * bool) =
                (let (t: string * bool) = (e,false) in t)
        in
        List.map func varList;;

let rec generateNextAssignList (assignList : (string * bool) list) : (string * bool) list * bool = 
        
        match assignList with
        [] -> ([], true)
       |h::t -> let temp = (generateNextAssignList t) in
                let hBool = match h with
                                (s,b) -> b
                in
                let hString = match h with
                                (s,b) -> s
                in
                let tempList = match temp with
                                (lst, someBool) -> lst
                in
                let tempBool = match temp with
                                (lst, someBool) -> someBool
                in
                if tempBool then ((hString, not(hBool))::tempList, hBool) else (h::tempList, tempBool);;
                
        
let lookupVar (assignList : (string * bool) list) (str : string) : bool = 

        let rec helper (lst : (string * bool) list) (target:string) : bool =

                match lst with
                [] -> false
                |h::t -> match h with 

                        (s,b) -> if (s = target) then b else (helper t target) 
        in
          
        (helper assignList str);; 



let rec evaluateTree (t : tree) (assignList : (string * bool) list) : bool = 
        

        match t with
        TreeNode (node, childrenList) ->
                                                         
                   match childrenList  with 
                       [] -> if (isAlphabet (node)) then (lookupVar (assignList) (node)) else (if (isBool node) then (boolOfString node) else false)
                       |h::[] -> if (node = "not") then (not (evaluateTree h (assignList))) else false 
                       |e1 :: e2 :: [] -> if (node = "and") then ((evaluateTree (e1) (assignList)) && (evaluateTree (e2) (assignList))) 
                                                        else ((evaluateTree (e1) (assignList)) || (evaluateTree (e2) (assignList)))
                       |e3::e4:: _ -> raise (Invalid_argument "Error");;                        
                                                                                                                                                                                      
let rec isLastPermutation (input: (string * bool) list): bool =

               match input with
               [] -> true
               |h::t -> (match h with
                        (s,b) -> if b then (isLastPermutation t) else false);;

                         
let rec satisfiableHelper (at: tree) (assignList: (string * bool) list) (accum: (string * bool) list list) : (string * bool) list list =
                
        if (isLastPermutation assignList) then (if (evaluateTree at assignList) then (assignList :: accum) else accum) else

                (let newAccum = if (evaluateTree at assignList) then (assignList :: accum) else accum 
                in
                let newAssignList = match (generateNextAssignList assignList) with
                                        (nextList,b)-> nextList 
                in 
                (satisfiableHelper at newAssignList newAccum));;

let satisfiable (input : string list) : (string * bool) list list =

        let t = (buildParseTree input) in
        let at = (buildAbstractSyntaxTree t) in
        let varString = (scanVariable input) in
        let initialList = (generateInitialAssignList varString)
        in
        (satisfiableHelper at initialList []);;




