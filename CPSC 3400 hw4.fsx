// Homework 4 for CPSC 3400, Spring 2021
// Solution by Kyle Fraser 


// Solution for exercise #1 

// Maps the computation done in compute volume to each tuple in the list. 
// Returns a list of computed volumes. 
let rec map computeVolume valueList = 
    match valueList with 
    | [] -> []
    | hd :: tl -> computeVolume hd::map computeVolume tl

// Computes the volume using two floating point values which represent radius and height. 
let computeVolume (r,h) = 
    System.Math.PI * r**2.0 * h

// Finds the max volume in a list of volumes. 
let rec findMax converted = 
    match converted with 
    | [] -> 0.0
    | hd :: tl -> 
            let max = findMax tl 
            if max > hd
               then max
               else hd

// Takes a list of floating point tuples that represent the dimensions of a cylinder. 
// Returns the volume of the cylinder with the largest volume. Each tuple has two floating 
// point values that are greater than 0. The first value represents radius and the second 
// value represents height. If the list is empty, 0 will be returned. 
let maxCylinderVolume values = 
    let converted = map computeVolume values
    findMax converted


// Solution for exercise #2
    
// Returns the first element in a list. 
// Returns 0 if the list is empty. 
let getFirstElement elements = 
   match elements with
   | [] -> 0  
   | hd :: tl -> hd

// Returns a filtered list with consecutive duplicates removed. 
let rec elim acc l = 
    match l with
    | [] -> acc
    | hd :: tl -> if hd = getFirstElement tl
                      then
                      elim (acc) tl
                  else 
                      elim (hd :: acc) tl

// Reverses the list 
let rec reverse a l =
    match l with
    | [] -> a
    | hd :: tl -> reverse (hd :: a) tl

// Takes a list of integers and eliminates consecutive duplicates. 
// Replaces consecutive duplicates with a single instance of the value. 
// order is preserved and non-consecutive duplicates are unaffected. 
let elimDuplicates l = 
    let acc = []
    let a = []
    reverse a (elim acc l)

    
// Solution for Exercise #3

// Tree definition for exercise #3
type BST =
    | Empty
    | TreeNode of int * BST * BST


// Inserts an integer into a binary tree and return the resulting tree.
// If the value already exists in the tree, the tree is returned 
// without inserting the value. 
// The resulting tree may not be balanced. 
let rec insert value tree = 
    match tree with
    | Empty -> TreeNode(value, Empty, Empty)
    | TreeNode(v, left, right) when value = v -> tree 
    | TreeNode(v, left, right) when value < v -> TreeNode(v, insert value left, right)
    | TreeNode(v, left, right) when value > v -> TreeNode(v, left, insert value right)
    | TreeNode(_,_,_) as n -> n 
   
    
// Returns true if the value is in the tree and false otherwise.
let search value tree = 
    let rec searching value tree = 
        match tree with
        | Empty -> Empty
        | TreeNode(v, left, right) when value = v -> TreeNode(v, left, right)
        | TreeNode(v, left, right) when value < v -> searching value left   
        | TreeNode(v, left, right) when value > v -> searching value right
        | TreeNode(_,_,_) as n -> n 


    let newbst = searching value tree
    match newbst with 
    | Empty -> false 
    | TreeNode(v, left, right) when value = v -> true
    | TreeNode(_,_,_) -> false 
   
     
// Tests the value of each node with a boolean function and returns the number 
// of nodes which evaluate to true. 
let count func tree =
    let rec check func tree = 
        match tree with 
        | Empty -> 0 
        | TreeNode(v, left, right) when func v = true -> 1 + (check func left) + (check func right) 
        | TreeNode(v, left, right) when func v = false -> (check func left) + (check func right)
        | TreeNode(_,_,_) -> 0 
               
    let final = check func tree 
    final 
    
// Returns the number of nodes that contain even integers in the BST. 
let evenCount tree = count (fun x -> x % 2 = 0) tree 
    




