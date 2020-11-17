module Equal

data Equal : x -> y -> Type where
  Reflexive : Equal a a

bool : Equal Bool Bool
bool = Reflexive

true : Equal True True
true = Reflexive

two : Equal 2 (1 + 1)
two = Reflexive

-- Helpers

transitive : Equal a b -> Equal b c -> Equal a c
transitive Reflexive Reflexive = Reflexive

symmetric : Equal a b -> Equal b a
symmetric Reflexive = Reflexive

congruent : (f : t -> t) -> Equal a b -> Equal (f a) (f b)
congruent f Reflexive = Reflexive

-- Built-in equality
