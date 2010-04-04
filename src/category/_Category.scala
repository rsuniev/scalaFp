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

package category

import id._Id

trait _Category
        extends _Id {
  type Op[-X, +Y] <: Category[X, Y]

  trait Category[-X, +Y]
          extends Id[X, Y] {
    self: Op[X, Y] =>

    /**
     * basic instance functionality
     *
     * the resulting computation
     * does the computation this and
     * does the computation op_y2z
     * using the output value of this as input value of op_y2z
     */
    def >>>[Z](op_y2z: Op[Y, Z]): Op[X, Z]
  }
}
