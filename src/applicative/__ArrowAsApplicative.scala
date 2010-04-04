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

package applicative

import arrow._Arrow

/*
 * arrow computations
 * of the form Op[E, X] for fixed E
 * can be seen as
 * applicative computations
 */
trait __ArrowAsApplicative[E] {
  type _ArrowType <: _Arrow

  implicit val _arrow: _ArrowType

  type _Fun[+X] = _arrow.Op[E, X]

  def __arr[W, X] = _arrow._arr[W, X]

  def __diag = _arrow._diag[E]

  def __apply[W, X] = _arrow._apply[W, X]

  trait _ArrowAsApplicative
          extends _Applicative {
    type Fun[+X] <: ArrowAsApplicative[X]

    def _close[X]: _Fun[X] => Fun[X]

    /**
     * module functionality
     *
     * given y
     *
     * use
     * __arr(_ => y)
     * and
     * _close
     */
    def _pure[X] =
      y => _close(__arr(_ => y))

    trait ArrowAsApplicative[+X]
            extends Applicative[X] {
      self: Fun[X] =>

      def open: _Fun[X]

      /**
       * instance functionality
       *
       * duplicate E to (E, E)
       * transform (E, E) to (X => Y, E)
       * transform (X => Y, E) to (X => Y, X)
       * and do pure _apply computation
       *
       * and
       * _close
       */
      def *:[Y](fun_x2y: Fun[X => Y]) =
        _close(__diag >>> fun_x2y.open.fst >>> this.open.snd >>> __apply)
    }
  }
}