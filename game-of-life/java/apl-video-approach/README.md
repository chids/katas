This is a version of Conway's Game of Life[1] implemented in Java and modelled solely from a video[2] illustrating how
to implement the game in APL.


== Background
Saturday the 17th of September 2011 I attended a Coding Dojo[3] organised by Peter Lind (@peter_lind), facilitated by
Emily Bache (@emilybache) and hosted by Valtech Stockholm. The task for the sessions was to do Conway's Game of
Life[1]. While I've never implemented the Game prior to attending the Dojo I did remember an old and obscure video[2]
showing how to implement the game in APL. The day after the Dojo I decided to do an attempt at implementing the game
in Java using the approach shown in the video.



== Approach
The approach to calculate the next generation described in the video[2] uses vectors of numbers since these are easily
rotated, summarised, and'ed and or'ed in APL. During the Dojo I had a thought of attempting a fixed board implementation
using a BitSet which I never got around to try - hence I chose BitSet as the data structure for this solution. BitSets 
support and'ing and or'ing but the rotation used in APL to produce the generations that is combined to get the live
cells for the next generation is not supported by BitSet nor List types in java. The rotation in APL makes the board
wrap around its edges which is a nice feature to allow the simulation to continue for a longer period of time. 



== References, see also
1: http://en.wikipedia.org/wiki/Conway's_Game_of_Life
2: http://www.youtube.com/watch?v=a9xAKttWgP4
3: http://codingdojo.org/