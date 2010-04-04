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

import functor._Functor
import pure._Pure

trait _Applicative
        extends _Pure
                with _Functor {
  type Fun[+X] <: Applicative[X]

  trait Applicative[+X]
          extends Pure[X]
                  with Functor[X] {
    self: Fun[X] =>

    /**
     * basic instance functionality
     *
     * the resulting computation
     * does the computation this
     * does the computation fun_x2y and
     * applies the return function value of the computation fun_x2y
     * to the return value of the computation this
     *
     * the recommended use cases are
     *
     * _pure(x2y) *: fun_x
     * where x2y: X => Y
     * (see map below)
     *
     * (_pure(xy2z) *: fun_x) *: fun_y
     * where xy2z: X => Y => Z
     *
     * ((_pure(wxy2z) *: fun_ w) *: fun_x) *: fun_y
     * where wxy2z: W => X => Y => Z
     *
     * and so on ...
     *
     * note that the parenthesis are necessary because
     * the operator *: is right associative
     * and we use it in a left associative way
     */
    def *:[Y](fun_x2y: Fun[X => Y]): Fun[Y]

    /**
     * derived instance functionality
     *
     * the resulting computation
     * does the computation this
     * does the pure computation that simply returns x2y
     * applies the function value x2y
     * to the return value of the computation this
     */
    override def map[Y](x2y: X => Y) =
      _pure(x2y) *: this
  }
}
