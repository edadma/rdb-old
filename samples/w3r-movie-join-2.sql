SELECT act_fname, act_lname, role
  FROM (actor JOIN movie_cast ON actor.act_id = movie_cast.act_id)
    JOIN movie ON movie_cast.mov_id = movie.mov_id
  WHERE mov_title = 'Annie Hall'