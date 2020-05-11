/*

type Language = text in ["English", "French", "Ongota"]

type Country = text in ["UK", "US", "JP"]

type PosInt = integer > 0

entity movie {
  *mov_id: integer
  mov_title: text
  mov_year: integer
  mov_time: PosInt
  mov_lang: Language
  mov_dt_rel: date
  mov_rel_country: Country
}

entity actor {
  *act_id: integer
  act_fname: text
  act_lname: text
  act_gender: text
}

entity movie_cast {
  actor: actor
  movie: movie
  role: text
}

entity director {
  dir_fname: text
  dir_lname: text
  movies: [movie] (movie_direction)
}

entity genres {
  gen_title: text
}



 */