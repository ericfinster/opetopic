/**
  * Nbe.scala - Normalization by Evaluation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopictt.nbe

object Nbe {

  def error(msg: String) = 
    throw new IllegalStateException(msg)

  sealed trait Term
  case object Type extends Term
  case class Var(i: Int) extends Term
  case class Pi(u: Term, v: Term) extends Term
  case class App(u: Term, v: Term) extends Term
  case class Lam(t: Term) extends Term

  type Tm = Int => Term

  sealed trait Dom
  case object TypeD extends Dom
  case class PiD(a: Dom, f: Dom => Dom) extends Dom
  case class LamD(f: Dom => Dom) extends Dom
  case class NeD(tm: Tm) extends Dom

  //
  //  Domain helpers
  //

  def arrD(a: Dom, b: Dom): Dom = 
    PiD(a, _ => b)

  def appD(a: Dom, b: Dom): Dom = 
    a match {
      case LamD(f) => f(b)
      case _ => error("appD")
    }

  def varD(a: Dom, k: Int): Dom = 
    up(a, l => Var(l + k))

  //
  //  Reflection and reification
  //

  // Reflect a term into the semantic domain along a semantic type
  def up(a: Dom, tm: Tm): Dom = 
    a match {
      case PiD(u, f) => LamD(d => up(f(d), k => App(tm(k), down(u, d)(k))))
      case _ => NeD(tm)
    }

  // Reify a domain element representing a type (i.e. implicitly along TypeD)
  def downT(a: Dom): Tm = 
    (k : Int) => {
      a match {
        case TypeD => Type
        case PiD(u, f) => Pi(downT(u)(k), downT(f(varD(u, -(k+1))))(k+1))
        case NeD(t) => t(k)
        case _ => error("downT")
      }
    }

  // Reify a domain element along a semantic type
  def down(a: Dom, b: Dom): Tm = 
    (k : Int) => {
      (a, b) match {
        case (PiD(u, f), e) => {
          val d = varD(u, -(k+1))
          Lam(down(f(d), appD(e, d))(k+1))
        }
        case (TypeD, a) => downT(a)(k)
        case (_, NeD(t)) => t(k)
        case _ => error("down")
      }
    }

  //
  //  Environments
  //

  type Env = Int => Dom

  val emptyEnv: Env = 
    (k: Int) => error("unbound index: " + k)

  def ext(rho: Env, d: Dom): Env = 
    (k: Int) => if (k == 0) d else rho(k-1)

  //
  //  Evaluation
  //

  def eval(t: Term, rho: Env): Dom = 
    t match {
      case Type => TypeD
      case Var(i) => rho(i)
      case Pi(u, v) => PiD(eval(u, rho), d => eval(v, ext(rho, d)))
      case App(u, v) => appD(eval(u, rho), eval(v, rho))
      case Lam(t) => LamD(d => eval(t, ext(rho, d)))
    }

  //
  //  Identity valuation (!?!)
  //

  type Ctxt = List[Term]

  def upGAt(i: Int, gma: Ctxt): Env = 
    gma match {
      case Nil => emptyEnv
      case a :: as => {
        val rho = upGAt(i, as)
        ext(rho, varD(eval(a, rho),i - gma.length))
      }
    }

  def upG(gma: Ctxt): Env = 
    upGAt(gma.length, gma)

  //
  //  Normalization by evaluation
  //

  def nbe(gma: Ctxt, ty: Term, tm: Term): Term = {
    val g = upG(gma)
    down(eval(ty, g), eval(tm, g))(0)
  }

  def nbeT(gma: Ctxt, ty: Term): Term = {
    downT(eval(ty, upG(gma)))(0)
  }

}
