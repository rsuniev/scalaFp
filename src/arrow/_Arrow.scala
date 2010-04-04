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

package arrow

import category._Category

trait _Arrow
        extends _Category {
  type Op[-X, +Y] <: Arrow[X, Y]

  /**
   * basic module functionality
   * embedding a
   * function value
   * into the world of
   * computations
   *
   * the resulting pure computation transforms
   * an input value to
   * an output value simply
   * by applying the function value to it
   */
  def _arr[X, Y]: (X => Y) => Op[X, Y]

  /**
   * derived module functionality
   *
   * only effect is to
   * make a copy of a value
   */
  def _diag[X] =
    _arr((x: X) => (x, x))

  /**
   * derived module functionality
   *
   * only effect is to
   * swaps values
   */
  def _swap[X, Y] =
    _arr((x_y: (X, Y)) =>
      x_y match {case (x, y) => (y, x)})

  /**
   * derived module functionality
   *
   * only effect is to
   * apply a function value to a value
   */
  def _apply[X, Y] =
    _arr((x2y_x: (X => Y, X)) =>
      x2y_x match {case (x2y, x) => x2y(x)})

  /**
   * derived module functionality
   * only effect is to
   * bind a value to a function value
   */
  def _bind[X, Y] =
    _arr((x_x2y: (X, X => Y)) =>
      x_x2y match {case (x, x2y) => x2y(x)})

  /**
   * derived module functionality
   *
   * the resulting computation
   * does not transform its input value
   */
  override def _id[X] =
    _arr(x => x)

  trait Arrow[-X, +Y]
          extends Category[X, Y] {
    self: Op[X, Y] =>

    /**
     * basic instance functionality
     *
     * the resulting computation
     * transforms the first input value of type X to
     * a first output value of type Y
     * while doing nothing with the second input value of type Z
     *
     * it is instructive to think as the value of type Z
     * as a global value for this (computation)
     */
    def fst[Z]: Op[(X, Z), (Y, Z)]

    /**
     * derived instance functionality
     *
     * swapped version of fst
     */
    def snd[Z]: Op[(Z, X), (Z, Y)] =
      _swap >>> fst[Z] >>> _swap
  }
}
