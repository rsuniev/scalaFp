//
//              ____________________________________________________________________________________
//             /                                       ___                _________                /\
//            /                                       /  /\              /  ______/\              / /
//           /   _________   _________   _________   /  / /_________    /  /\_____\/________     / /
//          /   /  ______/\ /  ______/\ /_____   /\ /  / //_____   /\  /  /_/__    /  ___  /\   / /
//         /   /  /_____ \//  /\_____\/_\____/  / //  / /_\____/  / / /   ____/\  /  /__/ / /  / /
//        /   /_____   /\ /  / /      /  ___   / //  / //  ___   / / /  /\____\/ /  _____/ /  / /
//       /   _______/ / //  /_/___   /  /__/  / //  / //  /__/  / / /  / /      /  /\____\/  / /
//      /   /________/ //________/\ /________/ //__/ //________/ / /__/ /      /__/ /       / /
//     /    \________\/ \________\/ \________\/ \__\/ \________\/  \__\/       \__\/ v1.0  / /
//    /                                                                                   / /
//   /                       Scala Functional Programming Library                        / /
//  /                        author Luc Duponcheel      2009-2010                       / /
// /___________________________________________________________________________________/ /
// \___________________________________________________________________________________\/
//

package monoidal

import functor._Functor

trait _Monoidal
        extends _Functor {
  type Fun[+X] <: Monoidal[X]

  /**
   * basic module functionality
   *
   * do nothing computation returning no value
   */
  def _fun_u: Fun[Unit]

  trait Monoidal[+X]
          extends Functor[X] {
    self: Fun[X] =>

    /**
     * basic instance functionality
     *
     * the resulting computation
     * does the computation this
     * does the computation fun_y and
     * returns a tuple consisting of
     * the return value of the computation this
     * and
     * the return value of the computation fun_y
     */
    def **[Y](fun_y: Fun[Y]): Fun[(X, Y)]
  }
}
