SELECT act_fname, act_lname, mov_title, mov_year
  FROM (actor JOIN movie_cast ON actor.act_id = movie_cast.act_id)
    JOIN movie ON movie_cast.mov_id = movie.mov_id
  WHERE mov_year NOT BETWEEN 1990 and 2000