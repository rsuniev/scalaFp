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

/*
 * applicative computations
 * can be composed
 */
trait __ComposedApplicative {
  type _ApplicativeType1 <: _Applicative
  type _ApplicativeType2 <: _Applicative

  implicit val _applicative1: _ApplicativeType1
  implicit val _applicative2: _ApplicativeType2

  type _Fun1[+X] = _applicative1.Fun[X]
  type _Fun2[+X] = _applicative2.Fun[X]

  def __pure1[Y] = _applicative1._pure[Y]

  def __pure2[Y] = _applicative2._pure[Y]

  trait _ComposedApplicative
          extends _Applicative {
    type Fun[+X] <: ComposedApplicative[X]

    def _close[Y]: _Fun2[_Fun1[Y]] => Fun[Y]

    /**
     * module functionality
     *
     * given y
     *
     * first do pure1 of the first computation type
     * second do pure1 of the first computation type
     * and
     * _close
     */
    def _pure[Y] =
      y => _close(__pure2(__pure1(y)))

    trait ComposedApplicative[+X]
            extends Applicative[X] {
      self: Fun[X] =>

      def open: _Fun2[_Fun1[X]]

      /**
       * instance functionality
       *
       * first do *: of the first computation type
       * second do pure2 of the first computation type
       * and
       * _close
       */
      def *:[Y](fun_x2y: Fun[X => Y]) =
        _close(
          (__pure2(
            (fun1_x2y: _Fun1[X => Y]) =>
              (fun1_x: _Fun1[X]) =>
                fun1_x2y *: fun1_x)
                  *: fun_x2y.open)
                  *: this.open)
    }
  }
}
