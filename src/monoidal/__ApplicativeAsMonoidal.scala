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

package monoidal


import applicative._Applicative

/*
 * applicative computations
 * can be seen as
 * monoidal computations
 */
trait __ApplicativeAsMonoidal {
  type _ApplicativeType <: _Applicative

  implicit val _applicative: _ApplicativeType

  type _Fun[+X] = _applicative.Fun[X]

  def __pure[Y] = _applicative._pure[Y]

  trait _ApplicativeAsMonoidal
          extends _Monoidal {
    type Fun[+X] <: ApplicativeAsMonoidal[X]

    def _close[Y]: _Fun[Y] => Fun[Y]

    /**
     * module functionality
     *
     * use () value
     * and
     * _close
     */
    def _fun_u =
      _close(__pure[Unit](()))

    trait ApplicativeAsMonoidal[+X]
            extends Monoidal[X] {
      self: Fun[X] =>

      def open: _Fun[X]

      /**
       * instance functionality
       *
       * use (x => y => (x, y)) function
       * and
       * _close
       */
      def **[Y](fun_y: Fun[Y]) =
        _close(
          (__pure[X => Y => (X, Y)](x => y => (x, y))
                  *: this.open)
                  *: fun_y.open)
    }
  }
}