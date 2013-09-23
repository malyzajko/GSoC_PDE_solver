package pde;

package object pde {
  import model.expression._
  import ceres.smartfloat.SmartFloat
  implicit def double2Const(c: Double) = Const(SmartFloat(c))
  implicit def ciT2BFunction(bf: Tuple2[Condition, from]): BFunction =
    BFunction(bf._1, bf._2)

}
