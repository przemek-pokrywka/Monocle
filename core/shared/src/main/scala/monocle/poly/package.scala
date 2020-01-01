package monocle

package object poly {
  type Setter[A, B]   = PSetter[A, A, B, B]
  type Optional[A, B] = POptional[A, A, B, B]
  type Lens[A, B]     = PLens[A, A, B, B]
  type Prism[A, B]    = PPrism[A, A, B, B]
  type Iso[A, B]      = PIso[A, A, B, B]
}
