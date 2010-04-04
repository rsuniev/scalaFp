/*
              ____________________________________________________________________________________
             /                                       ___                _________                /\
            /                                       /  /\              /  ______/\              / /
           /   _________   _________   _________   /  / /_________    /  /\_____\/________     / /
          /   /  ______/\ /  ______/\ /_____   /\ /  / //_____   /\  /   \/__    /  ___  /\   / /
         /   /  /_____ \//  /\_____\/ \____/  / //  / / \____/  / / /   ____/\  /  /__/ / /  / /
        /   /_____   /\ /  / /      /  ___   / //  / //  ___   / / /  /\____\/ /  _____/ /  / /
       /   _______/ / //   \/___   /  /__/  / //  / //  /__/  / / /  / /      /  /\____\/  / /
      /   /________/ //________/\ /________/ //__/ //________/ / /__/ /      /__/ /       / /
     /    \________\/ \________\/ \________\/ \__\/ \________\/  \__\/       \__\/ v1.0  / /
    /                                                                                   / /
   /                       Scala Functional Programming Library                        / /
  /                        author Luc Duponcheel      2009-2010                       / /
 /___________________________________________________________________________________/ /
 \___________________________________________________________________________________\/

*/

package functor

trait _Functor {
  type Fun[+X] <: Functor[X]

  /**
   * derived module functionality
   *
   * module level version of map
   */
  def _map[X, Y]: (X => Y) => Fun[X] => Fun[Y] =
    x2y => _.map(x2y)

  trait Functor[+X] {
    self: Fun[X] =>

    /**
     * basic instance functionality
     *
     * the resulting computation
     * does the computation this and
     * applies the function value x2y
     * to the return value of the computation this 
     */
    def map[Y](x2y: X => Y): Fun[Y]
  }
}