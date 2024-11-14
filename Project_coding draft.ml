(* OrderedType specification *)
module type OrderedType = 
sig
  type t
  val compare : t -> t -> int
end
 
(* AVL Tree Map Implementation *)
module AVLTreeMapFn (Ord : OrderedType) = 
struct
  (* Recursive definition *)
  type key = Ord.t
  type height = int 
  type 'v avl_tree =
    | Empty
    | Node of 'v avl_tree * (key * 'v) * height * 'v avl_tree
 
  (* Get function *)
  let rec get (key : key) (avl_tree : 'v avl_tree) : 'v option =
    match avl_tree with
    | Empty -> None
    | Node (left_subtree, (current_key, current_val), _, right_subtree) ->
        let cmp = Ord.compare key current_key in
        if cmp = 0 then
          Some current_val
        else if cmp < 0 then
          get key left_subtree
        else
          get key right_subtree
 
  (* Min function *)
  let rec min (avl_tree : 'v avl_tree) : 'v option =
    match avl_tree with
    | Empty -> None
    | Node (left_subtree, (_, current_val), _, _) ->
        match left_subtree with
        | Empty -> Some current_val
        | _ -> min left_subtree
 
  (* Max function *)
  let rec max (avl_tree : 'v avl_tree) : 'v option =
    match avl_tree with
    | Empty -> None
    | Node (_, (_, current_val), _, right_subtree) ->
        match right_subtree with
        | Empty -> Some current_val
        | _ -> max right_subtree
 
  (* Successor function *) 
  let successor (avl_tree : 'v avl_tree) (key : key) : key option =
    let rec helper node successor =
      match node with
      | Empty -> successor
      | Node (left_subtree, (current_key, _), _, right_subtree) ->
          let cmp = Ord.compare key current_key in
          if cmp < 0 then
            helper left_subtree (Some current_key)
          else 
            helper right_subtree successor
    in
    helper avl_tree None;;
 
  (* Predecessor function *)
  let predecessor (avl_tree : 'v avl_tree) (key : key) : key option =
    let rec helper node predecessor =
      match node with
      | Empty -> predecessor
      | Node (left_subtree, (current_key, _), _, right_subtree) ->
          let cmp = Ord.compare key current_key in
          if cmp > 0 then
            helper right_subtree (Some current_key)
          else 
            helper left_subtree predecessor
    in
    helper avl_tree None;;
 
  (*height function *)
  let height (t: 'a avl_tree) =
    match t with
    | Empty -> 0
    | Node (_, _, h, _) -> h
      
  (*max height function *)
  let max_h (t1: 'a avl_tree) (t2: 'a avl_tree)=
    match t1,t2 with
    | Empty,_ -> height t2
    | _,Empty -> height t1
    | _,_   -> if  height t1 <= height t2 then height t1
        else height t2
  
  (* create a node with updated height *)
  let make_node (left:'a avl_tree) (key:key) value right  =
    let h = 1 + max_h left right in
    Node (left, (key, value), h, right)
      
  (*rotate left function*)
  let rotate_left = function
    | Node (left, (key, value), _, Node (right_left, (right_key, right_value), _, right_right)) ->
        let new_left = make_node left key value right_left in
        make_node new_left right_key right_value right_right
    | node -> node (* Return the node unchanged if rotation is invalid *)

  (*rotate left function*)
  let rotate_right = function
    | Node (Node (left_left, (left_key, left_value), _, left_right), (key, value), _, right) ->
        let new_right = make_node left_right key value right in
        make_node left_left left_key left_value new_right
    | node -> node (* Return the node unchanged if rotation is invalid *)


  (* Balance factor of a node *)
  let balance_factor (t :'a avl_tree) = 
    match t with
    | Empty -> 0
    | Node (left, _, _, right) -> height left - height right

  (* Rebalance function *)
  let rebalance node =
    match node with
    | Empty -> Empty
    | Node (left, (key, value), _, right) as n ->
        let bf = balance_factor n in
        if bf > 1 then
          if height left >= height (match left with Node (_, _, _, r) -> r | _ -> Empty) then
            rotate_right n (* Left-Left case *)
          else
            rotate_right (make_node (rotate_left left) key value right) (* Left-Right case *)
        else if bf < -1 then
          if height right >= height (match right with Node (l, _, _, _) -> l | _ -> Empty) then
            rotate_left n (* Right-Right case *)
          else
            rotate_left (make_node left key value (rotate_right right)) (* Right-Left case *)
        else
          n (* Balanced *)

(* add function*)
  let rec add (key : key) (value : 'v) (avl_tree : 'v avl_tree) : 'v avl_tree =
    match avl_tree with
    | Empty -> Node (Empty, (key, value), 1, Empty) (* Insert new node if tree is empty *)
    | Node (left, (k, v), h, right) ->
        let cmp = Ord.compare key k in
        if cmp < 0 then
          rebalance (make_node (add key value left) k v right) (* Insert in left subtree *)
        else if cmp > 0 then
          rebalance (make_node left k v (add key value right)) (* Insert in right subtree *)
        else
          Node (left, (key, value), h, right) (* Key already exists, update value *)
                                              
(* Find the minimum node in a tree *)
  let rec min_node = function
    | Empty -> None
    | Node (Empty, (key, value), _, _) -> Some (key, value)
    | Node (left, _, _, _) -> min_node left

(* Remove function *)
  let rec remove (key : key) (avl_tree : 'v avl_tree) : 'v avl_tree =
    match avl_tree with
    | Empty -> Empty (* Key not found *)
    | Node (left, (k, v), h, right) ->
        let cmp = Ord.compare key k in
        if cmp < 0 then
          rebalance (make_node (remove key left) k v right) (* Remove from left subtree *)
        else if cmp > 0 then
          rebalance (make_node left k v (remove key right)) (* Remove from right subtree *)
        else (* Key found *)
          match left, right with
          | Empty, Empty -> Empty (* Node has no children *)
          | Empty, _ -> right (* Node has only right child *)
          | _, Empty -> left (* Node has only left child *)
          | _ -> (* Node has two children *)
              match min_node right with
              | Some (succ_key, succ_val) ->
                  rebalance (make_node left succ_key succ_val (remove succ_key right))
              | None -> failwith "Unexpected case in remove"

end
