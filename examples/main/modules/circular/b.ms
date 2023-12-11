import a

export const SECRET_B_VALUE: int = 42

export add: fn() -> int = fn() -> int {
  return a.SECRET_A_VALUE + SECRET_B_VALUE
}