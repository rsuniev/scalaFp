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

package traversable

import functor._Functor
import monoidal._Monoidal

/*
 * structure that can be traversed
 */

trait _Traversable
        extends _Functor {
  /*
  * Fun is a type function turning a
  * type
  * into a
  * structure that can be traversed
  */
  type Fun[+X] <: Traversable[X]

  /*
   * computation type that is used while traversing
   */
  type _MonoidalType <: _Monoidal

  implicit val _monoidal: _MonoidalType

  type _Fun[+X] = _monoidal.Fun[X]

  /**
   * basic module functionality
   *
   * applying the function value to a
   * structure of computations
   * results in a computation that
   * traverses the structure of computations
   * does all the visited computations and
   * returns the returned values, collected
   * in a way that they can be traversed
   */
  def _sequence[Y]: Fun[_Fun[Y]] => _Fun[Fun[Y]] =
    _.traverse(fun_y => fun_y)

  trait Traversable[+X]
          extends Functor[X] {
    self: Fun[X] =>

    /**
     * basic instance functionality
     *
     * the resulting computation
     * traverses the structure of values this and
     * for all visited values and all computations
     * resulting from applying a function value to them
     * does all the computations and
     * returns the returned values, collected
     * in a way that they can be traversed
     */
    def traverse[Y](x2fun_y: X => _Fun[Y]): _Fun[Fun[Y]]
  }
}
