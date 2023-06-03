package com.github.mercurievv.s4snosj
import cats.implicits.*
import cats.Monad
import org.scalacheck.cats.implicits.*
import org.scalacheck.{Arbitrary, Gen}

object ScalacheckDerivation:
  given [T: Arbitrary]: Gen[T] = summon[Arbitrary[T]].arbitrary
  extension [T](a: Gen[T])
    def toArbitrary: Arbitrary[T] = Arbitrary.apply(a)

  import magnolia1.*

  type TC[X] = Gen[X]

  given [M[_]](using CM: Monad[M]): Monadic[M] =
    new Monadic[M]:
      override def point[A](value: A): M[A]                       = CM.point(value)
      override def map[A, B](from: M[A])(fn: A => B): M[B]        = CM.map(from)(fn)
      override def flatMap[A, B](from: M[A])(fn: A => M[B]): M[B] = CM.flatMap(from)(fn)

  object ArbitraryDer extends AutoDerivation[TC]:
    override def join[T](ctx: CaseClass[TC, T]): TC[T] = ctx.constructMonadic(param => param.typeclass)
    override def split[T](ctx: SealedTrait[TC, T]): TC[T] = ctx.subtypes.map(_.typeclass).toList.sequence.flatMap(v => Gen.oneOf(v))
