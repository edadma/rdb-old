SELECT dir_fname, dir_lname, mov_title
  FROM ((director JOIN movie_direction ON director.dir_id = movie_direction.dir_id)
    JOIN movie ON movie_direction.mov_id = movie.mov_id)
      JOIN movie_cast ON movie_cast.mov_id = movie.mov_id
  WHERE role = 'Sean Maguire'