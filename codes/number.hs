{-
http://www.ibm.com/developerworks/java/library/j-ft3/index.html


Unit testing

Even though I haven't shown the unit tests for previous versions, all the examples have tests. An effective unit-testing library named ScalaTest is available for Scala (see Resources). Listing 2 shows the first unit test I wrote to verify the isPerfect() method from Listing 1:
Listing 2. A unit test for the Scala number classifier

@Test def negative_perfection() {
  for (i <- 1 until 10000)
    if (Set(6, 28, 496, 8128).contains(i))
      assertTrue(NumberClassifier.isPerfect(i))
    else
      assertFalse(NumberClassifier.isPerfect(i))
}

But like you, I'm trying to learn to think more functionally, and the code in Listing 2 bothered me in two ways. First, it iterates to do something, which exhibits imperative thinking. Second, I don't care for the binary catch-all of the if statement. What problem am I trying to solve? I need to make sure that my number classifier doesn't identify an imperfect number as perfect. Listing 3 shows the solution to this problem, stated a little differently:
Listing 3. Alternate test for perfect-number classification

@Test def alternate_perfection() {
  assertEquals(List(6, 28, 496, 8128),
              (1 until 10000) filter (NumberClassifier.isPerfect(_)))
}
-}

isFactor number pontentialFactor = mod number pontentialFactor == 0
suml alist = foldl (+) 0 alist
factors  number = filter (\f -> mod number f == 0) [1..number]
tester number = sum(factors(number)) - number

isPerfect number =  tester number == number
isAbundant  number = tester number > number
isDeficient number = tester number < number


unitTesting = filter isPerfect [1..10000]  == [6, 28, 496, 8128]

-- Unit Testing

