What does the function map (map (+1)) do?

Let's start with the type.
We know that the type of map is (a -> b) -> [a] -> [b]. Using this, we can infere the type of the entire application. Of course this is a curried function, and, since no second argument is provided, is still waiting for the second parameter. 
By the way, starting from the (+1) section we can infere the type of the function in a bottom up way. The type of the (+1) section is: Num a => a -> a
Thus, the type of the first application of map would be: Num a => (a -> a) -> [a] -> [a]. At this point, we notice that this function is waiting for a second anrgument, since the first is (+1). Thus, the type of the first application of the map function is [a] -> [a]. This is perfect, because it matches with the first argument of the second application of the map function (recall (a -> b)). Using this and proceeding in the same way of before, the type of the second map would be ([a] -> [a]) -> [[a]] -> [[a]]. Again, this application is curried and the first argument is provided (map (+1)), so the type of map (map (+1)) is [[a]] -> [[a]].

So, what does the function do?
It's quite simple. This function takes a list of list of a ([[a]]), where a is a type variable which represents a type in the Num type class. This means that each element of the main list is a list. For each element in the main list, the function sums 1. The elements of the second list are 'a', and since a is in the Num type class, the operator + is defined. An example of the application follows:
map (map (1+)) [[1,2], [3,4], [5,6], [10]] = [[2,3],[4,5],[6,7],[11]]

Generalizing the behaviour, given map (map (f)), in which f is a funcion with type a -> b, the final type is [[a]] -> [[b]]. The function takes a list of list of a (possibly with some contraints, like the Num one) and for each element of for each sublist in the main list, applies the function f.
