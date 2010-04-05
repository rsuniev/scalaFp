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

import applicative._Applicative

/*
 * monads are computations that can be programmed
 * using for comprehension syntax
 *
 * a typical use case is
 *
 * for{x <- f_x ; y <- x2f_y(x)} yield y
 *
 * note that the return value x of the computation f_x
 * can have an influence on the computation x2f_y(x)
 */

trait _Monad
        extends _Applicative {
  type Fun[+X] <: Monad[X]

  trait Monad[+X]
          extends Applicative[X] {
    self: Fun[X] =>

    /**
     * basic instance functionality
     *
     * the resulting computation is obtained by
     * doing the computation this and
     * applying the function value x2fun_y
     * to the return value of the computation this
     *
     * note that the return value of the computation this
     * can have an influence on the resulting computation
     */
    def flatMap[Y](x2fun_y: X => Fun[Y]): Fun[Y]

    /**
     * derived instance functionality
     *
     * the resulting computation
     * does the computation this
     * returning x
     * does the computation fun_x2y
     * returning x2y and
     * returns x2y(x)
     *
     * note that the return value of the computation this
     * has no influence on the computation fun_x2y but
     * can have an influence on the return value
     * of the resulting computation 
     */
    override def *:[Y](fun_x2y: Fun[X => Y]) =
      for{
        x <- this
        x2y <- fun_x2y
      } yield x2y(x)
  }
}
