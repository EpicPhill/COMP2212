
type word = bytes
type lang = word list

val append : lang -> char -> lang
val order : lang -> lang
val contains : lang -> word -> bool
val union : lang -> lang -> lang
val intersection : lang -> lang -> lang
val removedupes : lang -> lang
