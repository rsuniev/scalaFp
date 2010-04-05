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

package monad

import arrow._ApplicativeArrow
import applicative.__ArrowAsApplicative

/*
 * applicative arrow computations
 * of the form Op[Unit, X]
 * can be seen as
 * monadic computations
 */
trait __ApplicativeArrowAsMonad
        extends __ArrowAsApplicative[Unit] {
  type _ArrowType <: _ApplicativeArrow

  def __app[X] = _arrow._app[Unit, X]

  trait _ApplicativeArrowAsMonad
          extends _ArrowAsApplicative
                  with _Monad {
    type Fun[+X] <: ApplicativeArrowAsMonad[X]

    trait ApplicativeArrowAsMonad[+X]
            extends ArrowAsApplicative[X]
                    with Monad[X] {
      self: Fun[X] =>

      /**
       * instance functionality
       *
       * transform Unit to X
       * transform X to (Op(Unit, X), Unit)
       * and do _app computation
       *
       * and
       * _close
       */
      def flatMap[Y](x2fun_y: X => Fun[Y]) =
        _close(this.open >>> __arr(x => (x2fun_y(x).open, ())) >>> __app)
    }
  }
}


