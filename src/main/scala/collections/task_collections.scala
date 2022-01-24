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
    import java.text.DecimalFormat
    val tensNames = Array("", " ten", " twenty", " thirty", " forty", " fifty", " sixty", " seventy", " eighty", " ninety")
    val numNames = Array("", " one", " two", " three", " four", " five", " six", " seven", " eight", " nine", " ten",
      " eleven", " twelve", " thirteen", " fourteen", " fifteen", " sixteen", " seventeen", " eighteen", " nineteen")

    def convertLessThanOneThousand(number: Int): String = {
      val (soFar, num) = if (number % 100 < 20) {
        (numNames(number % 100), number / 100)
      } else {
        val soFar0 = numNames(number % 10)
        val soFar = tensNames(number % 10) + soFar0
        val num = number / 10
        (soFar, num)
      }
      if (num == 0) {
        soFar
      } else numNames(num) + " hundred" + soFar
    }


    def convert(number: Long): String = { // 0 to 999 999 999 999
      def getNumberPart(number: Int, decimalName: String) = {
        number match {
          case 0 => ""
          case 1 => convertLessThanOneThousand(number) + decimalName
          case _ => convertLessThanOneThousand(number) + decimalName
        }
      }

      if (number == 0) {
        "zero"
      } else {
        // pad with "0"
        val result = new StringBuilder()
        val mask = "000000000000"
        val df = new DecimalFormat(mask)
        val snumber = df.format(number)
        // XXXnnnnnnnnn
        val billions = snumber.substring(0, 3).toInt
        // nnnXXXnnnnnn
        val millions = snumber.substring(3, 6).toInt
        // nnnnnnXXXnnn
        val hundredThousands = snumber.substring(6, 9).toInt
        // nnnnnnnnnXXX
        val thousands = snumber.substring(9, 12).toInt
        result.append(getNumberPart(billions, " billion "))
        result.append(getNumberPart(millions, " million "))
        result.append(getNumberPart(hundredThousands, " thousand "))
        result.append(convertLessThanOneThousand(thousands))
        // remove extra spaces!
        result.toString.replaceAll("^\\s+", "").replaceAll("\\b\\s{2,}\\b", " ")
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
