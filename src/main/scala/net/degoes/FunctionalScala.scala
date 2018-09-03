package net.degoes

import scalaz.zio._
import scalaz.zio.console._

object effects_as_values {
  sealed trait Program[+A] { self =>

    def map1[B](f: A => B): Program[B] = self match {
      case Return(a) => Return(f(a))
      case GetStrLine(next) => GetStrLine((a:String) => next(a).map(f))
      case _ => ???
    }

    def map[B](f: A => B): Program[B] = flatMap(a => Return(f(a)))

    def flatMap[B](f: A => Program[B]): Program[B] = self match {
      case Return(a) => f(a)
      case x => Chain(self, f)
    }
  }
  object Program {
    def point[A](a : => A): Program[A] = Return(a)
  }

  final case class Return[A](value: A) extends Program[A]
  final case class GetStrLine[A](next: String => Program[A]) extends Program[A]
  final case class PutStrLine[A](line: String, next: Program[A]) extends Program[A]
  final case class Chain[A0, A](previous: Program[A0], next: A0 => Program[A]) extends Program[A]

  // def and val are equivalent here, because referential transparency
  // => no change of meaning
  val getStrLine: Program[String] = GetStrLine(Program.point(_))
  // () : Unit
  // f(s: String) ====> f "plop"  <=> Return ()
  // f(s: String) ====> f("plop")  <=> Return(())

  def putStrLine(line: String): Program[Unit] = PutStrLine(line, Return(()))

  val myPogram: Program[Unit] = {
    for {
      _    <- putStrLine("Welcome to my purely functional program!")
      _    <- putStrLine("What is your name?")
      name <- getStrLine
      _    <- putStrLine("Hello, " + name + ", it is good to see you!")
    } yield ()
  }
}

object FunctionalScala extends App {
  def run(args: List[String]): IO[Nothing, ExitStatus] =
    (for {
      _ <- putStrLn("Hello World!")
    } yield ()).redeemPure(_ => ExitStatus.ExitNow(1), _ => ExitStatus.ExitNow(0))
}
