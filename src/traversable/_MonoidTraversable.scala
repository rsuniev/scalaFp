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

import monoidal._MonoidAsMonoidal

/*
 * traversable structure
 * using monoids as monoidals
 */

trait _MonoidTraversable
        extends _Traversable {
  type _MonoidalType <: _MonoidAsMonoidal

  type __M = _monoidal._M

  def __close[Y] = _monoidal._close[Y]

  /**
   * derived module functionality
   *
   * while traversing monoid values
   * fold them
   * to a resulting monoid value
   */
  def _fold =
    (fun_m: Fun[__M]) => _sequence(_map(__close)(fun_m)).open

  trait MonoidTraversable[+X]
          extends Traversable[X] {
    self: Fun[X] =>

    /**
     * derived instance functionality
     *
     * while traversing values
     * fold the results of applying a monoid valued function to them
     * to a resulting monoid value
     */
    def accumulate(x2m: X => __M) =
      traverse(x => __close(x2m(x))).open
  }
}