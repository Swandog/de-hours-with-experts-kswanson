package com.labs1904

object NextBiggestNumber {

  def main(args: Array[String]) = {

    val input = args(0).toInt
    val nextBiggestNumber = getNextBiggestNumber(input)
    println(s"Input: $input")
    println(s"Next biggest number: $nextBiggestNumber")
  }

  def getNextBiggestNumber(i: Integer) = {
    // The next biggest number will be found by rearranging
    // the longest sequence of declining digits that includes the final digit
    // and the digit just before that

    // First, split the number into digits
    val digits = i.toString().toList.map(_.asDigit)

    // reverse the sequence of digits
    val digitsReversed = digits.reverse

    // find the spot where the revesed digits stop increasing
    val pivotPoint = digitsReversed.sliding(2).indexWhere(x => x(0) > x(1)) + 1

    if(pivotPoint > 0) {
      // The pivot point is the start of the digits we want to change
      // Split the (reversed) list into numbers not changing, and those that are
      val (changing, pivot :: reversedUnchanged) = digitsReversed.splitAt(pivotPoint)
      val (starting, newPivot :: ending) = changing.sorted.span(_ <= pivot)

      val newDigits = reversedUnchanged.reverse ++ (newPivot :: starting ++ (pivot :: ending))

      val answer = newDigits.mkString.toInt
      answer
    } else {
      // If the pivotPoint is 0, that means indexWhere returned -1, which means
      // the revesed digits only increased, which means that the original sequence
      // only decreased. In this case, a higher number cannot be found
      -1
    }
  }
}
