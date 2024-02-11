# Write a function that does ProgR-course homework grading
#
# The function takes two arguments, `problems` and `completion`:
# * `problems`: a character vector listing the available exercise problems.
#
#   Exercise problems given in the format "rfile.R:ex01FunctionName" -- the R-file,
#   followed by a colon, followed by the function name of the exercise. The '.R'
#   part is non-essential here, but the colon is, separating the "problem set"
#   from the "problem".
#
#   The `problems`-vector for the second homework would for example be:
problems.example <- c(
  "01_exercise_getting_started.R:ex01Multiply",
  "01_exercise_getting_started.R:ex02MultiplyVectors",
  "01_exercise_getting_started.R:ex03VectorType",
  "01_exercise_getting_started.R:ex04Odd",
  "02_exercise_vectors.R:ex01BinCounting",
  "02_exercise_vectors.R:ex02Binning",
  "02_exercise_vectors.R:ex03FizzBuzz",
  "03_exercise_matrices_and_dataframes.R:ex01ChessboardSum",
  "03_exercise_matrices_and_dataframes.R:ex02MatrixWhichMin",
  "03_exercise_matrices_and_dataframes.R:ex04SelectDF",
  "03_exercise_matrices_and_dataframes.R:ex05Imputation",
  "04_control_structures.R:ex01Opposite",
  "04_control_structures.R:ex02CellularAutomaton"
)
# * `completion`: a `data.frame` with two columns
#   - `student`: a character column indicating the student
#   - `solved`: a character column indicating the problems
#     completed successfully by the student.
#   The `completion` parameter contains one row per student and completed
#   exercise. If "studentA" has solved all problems from 01_exercise_vectors.R
#   and the first three problems of 02_exercise_matrices_and_dataframes.R, while
#   "studentB" has only solved the Counterdiagonal and the CellularAutomaton problem
#   then the input `completion` argument could look like this:
completion.example <- data.frame(
    student = c("studentA", "studentA", "studentB", "studentA", "studentA",
      "studentA", "studentA", "studentB", "studentA", "studentA", "studentA", "studentA"),
    solved = c("01_exercise_getting_started.R:ex02MultiplyVectors",
      "02_exercise_vectors.R:ex03FizzBuzz",
      "04_control_structures.R:ex02CellularAutomaton",
      "01_exercise_getting_started.R:ex01Multiply",
      "01_exercise_getting_started.R:ex03VectorType",
      "01_exercise_getting_started.R:ex04Odd",
      "02_exercise_vectors.R:ex02Binning",
      "03_exercise_matrices_and_dataframes.R:ex01ChessboardSum",
      "03_exercise_matrices_and_dataframes.R:ex02MatrixWhichMin",
      "03_exercise_matrices_and_dataframes.R:ex01ChessboardSum",
      "02_exercise_vectors.R:ex01BinCounting",
      "01_exercise_getting_started.R:ex03VectorType"),
    stringsAsFactors = FALSE
)
# The function should return a named `numeric` vector enumerating the scores of the students in any order.
#
# Scoring works as follows: Each student gets 0.25 points for each problem set (i.e. .R-file) that
# they solved completely. In the example above, "studentA" would get 0.5 points because the
# 01_exercise_getting_started.R-set and the 02_exercise_vectors.R-set are complete.
# "studentB" would get 0 points. The return for this example should therefore be
# ex01ProgrGrading(problems = problems.example, completion = completion.example)
# --> c(studentA = 0.5, studentB = 0)
# or
# --> c(studentB = 0, studentA = 0.5)
#
# Other example calls:
# ex01ProgrGrading(
#   problems = problems.example,
#   completion = data.frame(student = character(0), solved = character(0), stringsAsFactors = FALSE)
# ) --> numeric(0)
# ex01ProgrGrading(
#   problems = problems.example,
#   completion = data.frame(student = "studentA", solved = "01_exercise_vectors.R:ex01Multiplication",
#     stringsAsFactors = FALSE)
# ) --> ERROR (because "01_exercise_vectors.R:ex01Multiplication" is not one of the problems)
# ex01ProgrGrading(
#   problems = c("A:one", "B:one"),
#   completion = data.frame(student = "studentA", solved = "A:one", stringsAsFactors = FALSE)
# ) --> c(studentA = 0.25)
# ex01ProgrGrading(
#   problems = c("A:one", "B"),
#   completion = data.frame(student = "studentA", solved = "A:one", stringsAsFactors = FALSE)
# ) --> ERROR ('problems' list is badly formatted, because there is no ':' in problem "B")
# ex01ProgrGrading(
#   problems = c("A:one", "B::one"),
#   completion = data.frame(student = "studentA", solved = "A:one", stringsAsFactors = FALSE)
# ) --> ERROR ('problems' list is badly formatted, because there are two ':' in problem "B")
# ex01ProgrGrading(
#   problems = c("A:one", "B:one", "A:two", "C:three"),
#   completion = data.frame(
#     student = c("studentA", "studentB", "studentC", "studentB", "studentC", "studentA"),
#     solved = c("C:three", "A:two", "C:three", "A:one", "B:one", "B:one"),
#     stringsAsFactors = FALSE)
# ) --> c(studentB = 0.25, studentC = 0.5, studentA = 0.5)
# You may find the `tapply()` function useful here.


ex01ProgrGrading <- function(problems, completion) {
  assertCharacter(problems, any.missing = FALSE, min.len = 1)
  assertDataFrame(completion, any.missing = FALSE)
  assertCharacter(completion$student, any.missing = FALSE)
  assertCharacter(completion$solved, any.missing = FALSE)
  validFormat <- grepl("^\\w+(\\.R)?:\\w+$", problems)
  if (any(!validFormat)) {
    stop("Invalid format detected in 'problems'. Expected format is 'filename:exercise' or 'filename.R:exercise'.")
  }
  if (nrow(completion) == 0) {
    return(numeric(0))
  }
  if (any(!grepl(":", problems))) {
    stop("'problems' list is badly formatted, because there is no ':' in some problems")
  }
  if (any(vapply(problems, function(x) sum(grepl(":", x)), integer(1)) > 1)) {
    stop("'problems' list is badly formatted, because there are two ':' in some problems")
  }
  if (any(!completion$solved %in% problems)) {
    problematicProblems <- paste(setdiff(completion$solved, problems), collapse = ", ")
    stop(paste("Assertion failed: ", problematicProblems, "are not one of the problems"))
  }
  problemSets <- unique(vapply(strsplit(problems, ":"), `[`, character(1), 1))
  studentScores <- vapply(unique(completion$student), function(currentStudent) {
    studentCompletion <- completion[completion$student == currentStudent, ]
    studentSets <- unique(vapply(strsplit(studentCompletion$solved, ":"), `[`, character(1), 1))
    completeSets <- vapply(problemSets, function(problemSet) {
      setProblems <- problems[startsWith(problems, problemSet)]
      all(setProblems %in% studentCompletion$solved)
    }, logical(1))
    sum(completeSets) * 0.25
  }, numeric(1))
  return(studentScores)
}