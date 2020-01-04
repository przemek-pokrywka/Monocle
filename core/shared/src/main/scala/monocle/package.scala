package object monocle {
  type UPrism[A, B]    = Prism[Unit, A, B]
  type UOptional[A, B] = Optional[Unit, A, B]
}
