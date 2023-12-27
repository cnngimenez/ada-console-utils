/* 
   Copyright 2023 Christian Gimenez
   
   Author: Christian Gimenez   

   files.c
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


#include <sys/stat.h>

struct stat_struct {
  int user_id, group_id;
  
  long access_time, modification_time, status_change_time;
};

void my_stat(char* path, struct stat_struct *statdata) {
  struct stat statfile;
  
  stat(path, &statfile);

  /* statdata->st_dev = statfile.st_dev; */
  /* statdata->st_ino = statfile.st_ino; */
  /* statdata->st_mode = statfile.st_mode; */
  /* statdata->st_nlink = statfile.st_nlink;   */
  statdata->user_id = statfile.st_uid;
  statdata->group_id = statfile.st_gid;
  /* statdata->st_rdev = statfile.st_rdev; */
  /* statdata->st_size = statfile.st_size; */
  /* statdata->st_blksize = statfile.st_blksize; */
  /* statdata->st_blocks = statfile.st_blocks;  */
  statdata->access_time = statfile.st_atim.tv_sec;
  statdata->modification_time = statfile.st_mtim.tv_sec;
  statdata->status_change_time = statfile.st_ctim.tv_sec; 
  
}
