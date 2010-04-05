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

import monoid._Monoid

/*
 * monoids can be seen as phantom monoidal computations
 * (computation that are completely ignored) 
 */
trait _MonoidAsMonoidal
        extends _Monoidal {
  type Fun[+X] <: MonoidAsMonoidal[X]

  type _MonoidType <: _Monoid

  implicit val _monoid: _MonoidType

  type _M = _monoid.M

  def _close[Y]: _M => Fun[Y]

  def __u = _monoid._u

  /**
   * module functionality
   */
  def _f_u =
    _close(__u)

  trait MonoidAsMonoidal[+X]
          extends Monoidal[X] {
    self: Fun[X] =>

    def open: _M

    /**
     * instance functionality
     */
    def **[Y](fun_y: Fun[Y]) =
      _close(this.open * fun_y.open)
  }
}
