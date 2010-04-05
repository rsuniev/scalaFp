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

package arrow

import monad._Monad

/*
 * monadic computations
 * of the form X => Fun[Y]
 * can be seen as
 * applicative arrow computations
 */
trait MonadAsApplicativeArrow__ {
  type _MonadType <: _Monad

  implicit val _monad: _MonadType

  type _Fun[+X] = _monad.Fun[X]

  def __pure[X] = _monad._pure[X]

  trait _MonadAsApplicativeArrow
          extends _ApplicativeArrow {
    type Op[-X, +Y] <: MonadAsApplicativeArrow[X, Y]

    def _close[X, Y]: (X => _Fun[Y]) => Op[X, Y]

    /**
     * module functionality
     *
     * given f
     *
     * use
     * x => __pure(f(x))
     * and
     * _close
     */
    def _arr[X, Y] =
      (f: X => Y) =>
        _close(x => __pure(f(x)))

    /**
     * module functionality
     *
     * use
     * { case ((op_x2y, x)) => op_x2y.open(x) }
     * and
     * _close
     */
    def _app[X, Y] =
      _close({case ((op_x2y, x)) => op_x2y.open(x)})

    trait MonadAsApplicativeArrow[-X, +Y]
            extends ApplicativeArrow[X, Y] {
      self: Op[X, Y] =>

      def open: X => _Fun[Y]

      /**
       * instance functionality
       *
       * use appropriate for comprehension
       * and
       * _close
       */
      def >>>[Z](op_y2z: Op[Y, Z]) =
        _close(x =>
          for{
            y <- this.open(x)
            z <- op_y2z.open(y)
          } yield z)

      /**
       * instance functionality
       *
       * use appropriate for comprehension
       * and
       * _close
       */
      def fst[Z] =
        _close({
          case ((x, z)) =>
            for{
              y <- this.open(x)
            } yield (y, z)
        })
    }
  }
}
