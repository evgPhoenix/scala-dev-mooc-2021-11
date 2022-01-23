package collections

import scala.util.{Failure, Success, Try}

object task_collections extends App {

  val text = "Hello. I am 9 years old"
  val input =  ("""\d+""".r findAllIn text).toList
  println(s"input = $input")
  println(text.substring(0, text.indexOf(input.head)) + input.head + text.substring(text.indexOf(input.head)+ input.head.length, text.length) )

  def isASCIIString(str: String): Boolean = str.matches("[A-Za-z]+")

  /**
   * Реализуйте метод который первый элемент списка не изменяет, а для последующих алгоритм следующий:
   * если isASCIIString is TRUE тогда пусть каждый элемент строки будет в ВЕРХНЕМ регистре
   * если isASCIIString is FALSE тогда пусть каждый элемент строки будет в нижнем регистре
   * Пример:
   * capitalizeIgnoringASCII(List("Lorem", "ipsum" ,"dolor", "sit", "amet")) -> List("Lorem", "IPSUM", "DOLOR", "SIT", "AMET")
   * capitalizeIgnoringASCII(List("Оказывается", "," "ЗвУк", "КЛАВИШЬ", "печатной", "Машинки", "не", "СТАЛ", "ограничивающим", "фактором")) ->
   * List("Оказывается", "," "звук", "КЛАВИШЬ", "печатной", "машинки", "не", "стал", "ограничивающим", "фактором")
   * HINT: Тут удобно использовать collect и zipWithIndex
   *
   * **/
  def capitalizeIgnoringASCII(text: List[String]): List[String] = {
    (List(text.head) ::: text.drop(1).map { el =>
      isASCIIString(el) match {
        case true => el.toUpperCase
        case false => el.toLowerCase
      }
    })
  }

  /**
   *
   * Компьютер сгенерировал текст используя вместо прописных чисел, числа в виде цифр, помогите компьютеру заменить цифры на числа
   * В тексте встречаются числа от 0 до 9
   *
   * Реализуйте метод который цифровые значения в строке заменяет на числа: 1 -> one, 2 -> two
   *
   * HINT: Для всех возможных комбинаций чисел стоит использовать Map
   * **/
  def numbersToNumericString(text: String): String = {

    val input =  ("""\d+""".r findAllIn text).toList
    println(s"input = $input")
    import java.text.DecimalFormat
    val tensNames = Array("", " ten", " twenty", " thirty", " forty", " fifty", " sixty", " seventy", " eighty", " ninety")

    val numNames = Array("", " one", " two", " three", " four", " five", " six", " seven", " eight", " nine", " ten", " eleven", " twelve", " thirteen", " fourteen", " fifteen", " sixteen", " seventeen", " eighteen", " nineteen")

    def convertLessThanOneThousand(number: Int): String = {
      var soFar = ""
      var num = number
      if (number % 100 < 20) {
        soFar = numNames(number % 100)
        num = number / 100
      }
      else {
        soFar = numNames(number % 10)
        num = number / 10
        soFar = tensNames(number % 10) + soFar
        num = number / 10
      }
      if (num == 0) {
        soFar
      } else numNames(num) + " hundred" + soFar
    }


    def convert(number: Long): String = { // 0 to 999 999 999 999
      if (number == 0) {
        "zero"
      } else {
        var snumber = number.toString
        // pad with "0"
        val mask = "000000000000"
        val df = new DecimalFormat(mask)
        snumber = df.format(number)
        // XXXnnnnnnnnn
        val billions = snumber.substring(0, 3).toInt
        // nnnXXXnnnnnn
        val millions = snumber.substring(3, 6).toInt
        // nnnnnnXXXnnn
        val hundredThousands = snumber.substring(6, 9).toInt
        // nnnnnnnnnXXX
        val thousands = snumber.substring(9, 12).toInt
        var tradBillions = ""
        billions match {
          case 0 =>
            tradBillions = ""

          case 1 =>
            tradBillions = convertLessThanOneThousand(billions) + " billion "

          case _ =>
            tradBillions = convertLessThanOneThousand(billions) + " billion "
        }
        var result = tradBillions
        var tradMillions = ""
        millions match {
          case 0 =>
            tradMillions = ""

          case 1 =>
            tradMillions = convertLessThanOneThousand(millions) + " million "

          case _ =>
            tradMillions = convertLessThanOneThousand(millions) + " million "
        }
        result = result + tradMillions
        var tradHundredThousands = ""
        hundredThousands match {
          case 0 =>
            tradHundredThousands = ""

          case 1 =>
            tradHundredThousands = "one thousand "

          case _ =>
            tradHundredThousands = convertLessThanOneThousand(hundredThousands) + " thousand "
        }
        result = result + tradHundredThousands
        var tradThousand = ""
        tradThousand = convertLessThanOneThousand(thousands)
        result = result + tradThousand
        // remove extra spaces!
        result.replaceAll("^\\s+", "").replaceAll("\\b\\s{2,}\\b", " ")
      }
    }

    Try(convert(input.head.toLong)) match {
      case Failure(_) => text
      case Success(value) =>
        //value
        text.substring(0, text.indexOf(input.head)) + value + text.substring(text.indexOf(input.head)+ input.head.length, text.length)
    }
  }

  /**
   *
   * У нас есть два дилера со списками машин которые они обслуживают и продают (case class Auto(mark: String, model: String)).
   * Базы данных дилеров содержат тысячи и больше записей. Нет гарантии что записи уникальные и не имеют повторений
   * HINT: Set
   * HINT2: Iterable стоит изменить
   * **/

  case class Auto(mark: String, model: String)

  /**
   * Хотим узнать какие машины можно обслужить учитывая этих двух дилеров
   * Реализуйте метод который примет две коллекции (два источника) и вернёт объединенный список уникальный значений
   **/
  def intersectionAuto(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Iterable[Auto] = {
    dealerOne.toSet intersect dealerTwo.toSet
  }

  /**
   * Хотим узнать какие машины обслуживается в первом дилеромском центре, но не обслуживаются во втором
   * Реализуйте метод который примет две коллекции (два источника)
   * и вернёт уникальный список машин обслуживающихся в первом дилерском центре и не обслуживающимся во втором
   **/
  def filterAllLeftDealerAutoWithoutRight(dealerOne: Iterable[Auto], dealerTwo: Iterable[Auto]): Iterable[Auto] = {
    dealerOne.toSet diff dealerTwo.toSet
  }
}
