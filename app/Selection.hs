module Selection where
import Solution ( Solution )
import Data.Vector ( (!), length, Vector )
import Random ( getRandomNumber )

selection :: Vector Solution -> Int -> IO (Solution, Solution)
selection solutions bestSolutionIndex =
  do
    let firstSolution = solutions ! bestSolutionIndex
    randomNumber <- getRandomNumber (0, toInteger $ Data.Vector.length solutions -2)
    let randomIndex = if randomNumber == bestSolutionIndex then randomNumber + 1 else randomNumber
        secondSolution = solutions ! 0
    return (firstSolution, secondSolution)

