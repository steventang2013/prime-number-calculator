
object PrimeNumberCalculator {

  def findPrimeNumbers(from: Int, to: Int): Seq[Int] = {
    // to must be >= 2 or else we do not have a valid sequence
    require(to >= 2)

    // filter all values between 2 and to that are primes (we don't care about from values < 2)
    if (from < 2)
      2.to(to).filter(x => isPrime(x)).toList
    else
      from.to(to).filter(x => isPrime(x)).toList
  }

  def isPrime(num: Int): Boolean = {
    // 2->sqrt(num) covers all factors of num; verify that no values within the range is divisible
    (2 to math.sqrt(num).toInt).forall (x => num % x != 0)
  }

}
