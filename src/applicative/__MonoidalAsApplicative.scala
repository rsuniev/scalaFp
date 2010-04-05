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

package applicative

import monoidal._Monoidal

/*
 * monoidal computations
 * can be seen as
 * applicative computations
 */
trait __MonoidalAsApplicative {
  type _MonoidalType <: _Monoidal

  implicit val _monoidal: _MonoidalType

  type _Fun[+X] = _monoidal.Fun[X]

  def __fun_u = _monoidal._fun_u

  def __map[X, Y] = _monoidal._map[X, Y]

  trait _MonoidalApplicative
          extends _Applicative {
    type Fun[+X] <: MonoidalApplicative[X]

    def _close[Y]: _Fun[Y] => Fun[Y]

    /**
     * module functionality
     *
     * given y
     *
     * apply
     * __map[Unit, Y](_ => y)
     * to
     * __fun_u
     * and
     * _close
     */
    def _pure[Y] =
      (y: Y) =>
        _close(__map[Unit, Y](_ => y)(__fun_u))

    trait MonoidalApplicative[+X]
            extends Applicative[X] {
      self: Fun[X] =>

      def open: _Fun[X]

      /**
       * instance functionality
       *
       * apply
       * __map[(X => Y, X), Y](  { case ((x2y, x)) => x2y(x) } )
       * to
       * fun_x2y.open ** this.open
       * and
       * _close 
       */
      def *:[Y](fun_x2y: Fun[X => Y]) =
        _close(
          __map[(X => Y, X), Y]
                    ({case ((x2y, x)) => x2y(x)})
                    (fun_x2y.open ** this.open))
    }
  }
}
