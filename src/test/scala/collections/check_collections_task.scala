package collections

import collections.task_collections._
import org.scalatest.flatspec.AnyFlatSpec

class check_collections_task extends AnyFlatSpec {

  "check capitalizeIgnoringASCII" should "ok" in  {
    println(isASCIIString("Lorem"))
    println(isASCIIString("ipsum"))
    println(capitalizeIgnoringASCII(List("Lorem", "ipsum", "dolor", "sit", "amet")))
    assert(capitalizeIgnoringASCII(List("Lorem", "ipsum", "dolor", "sit", "amet")) == List("Lorem", "IPSUM", "DOLOR", "SIT", "AMET"))
    assert(capitalizeIgnoringASCII(List("Оказывается", ",", "ЗвУк", "КЛАВИШЬ")) === List("Оказывается", ",", "звук", "клавишь"))
  }

  "check numbersToNumericString" should "ok" in {
    val text = "Hello. I am 9 years old"
    //val transformText = "Hello. I am ten years old" // it's a mistake -> should be nine
    val transformText = "Hello. I am nine years old" // it's a mistake -> should be nine
    assert(numbersToNumericString(text) === transformText)
    assert(numbersToNumericString("") === "")
    assert(numbersToNumericString("4") === "four")
    val text2 = "Hello. I am 100500 years old"
    assert(numbersToNumericString(text2) === "Hello. I am one hundred thousand five hundred years old")
  }

  "check intersectionAuto" should "ok" in {
    val dealerOne = Vector(Auto("BMW", "i3"), Auto("Mazda", "X5"))
    val dealerTwo = Seq(Auto("BMW", "i3"), Auto("Mazda", "X5"))
    assert(intersectionAuto(dealerOne, dealerTwo) === Set(Auto("BMW", "i3"), Auto("Mazda", "X5")))
  }

  "check filterAllLeftDealerAutoWithoutRight" should "ok" in {
    val dealerOne = Vector(Auto("BMW", "i3"), Auto("Mazda", "X5"))
    val dealerTwo = Seq(Auto("BMW", "i3"), Auto("Mazda", "X5"))
    assert(filterAllLeftDealerAutoWithoutRight(dealerOne, dealerTwo) === Set.empty)

    val dealerOneSecond = Vector(Auto("BMW", "i3"), Auto("Mazda", "X5"))
    val dealerTwoSecond = Seq(Auto("BMW", "i3"))
    assert(filterAllLeftDealerAutoWithoutRight(dealerOneSecond, dealerTwoSecond) === Set(Auto("Mazda", "X5")))
  }

}