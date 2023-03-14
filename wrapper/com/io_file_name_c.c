#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "io_file_name_c.h"

/** 並列計算用読み込みファイル名の取得（グローバルコミュニケータから指定） */
const char* monolis_get_global_input_file_name(
  const char* dir_name,
  const char* file_name)
{
  int commsize;
  int myrank;
  int buf_size = 1000;
  char* filename;

  commsize = monolis_get_global_commsize();
  myrank = monolis_get_global_myrank();
  filename = (char*)malloc(sizeof(char)*buf_size);

  if(commsize > 1){
    snprintf(filename, buf_size, "%s/%s.%d", "parted.0", filename_body, myrank);
  } else {
    snprintf(filename, buf_size, "%s", filename_body);
  }
  return filename;
}

/** 並列計算用読み込みファイル名の取得（ローカルコミュニケータから指定） */
const char* monolis_get_local_input_file_name(
  const char* dir_name,
  const char* file_name,
  int         comm)
{
  int commsize;
  int myrank;
  int buf_size = 1000;
  char* filename;

  commsize = monolis_get_global_commsize();
  myrank = monolis_get_global_myrank();
  filename = (char*)malloc(sizeof(char)*buf_size);

  if(commsize > 1){
    snprintf(filename, buf_size, "%s/%s.%d", "parted.0", filename_body, myrank);
  } else {
    snprintf(filename, buf_size, "%s", filename_body);
  }
  return filename;
}

/** 並列計算用書き出しファイル名の取得（グローバルコミュニケータから指定） */
const char* monolis_get_global_output_file_name(
  const char* dir_name,
  const char* file_name)
{
  int commsize;
  int myrank;
  int buf_size = 1000;
  char* head;
  char* post;
  char ctmp[buf_size], body[buf_size];
  char* filename;

  commsize = monolis_get_global_commsize();
  myrank = monolis_get_global_myrank();
  filename = (char*)malloc(sizeof(char)*buf_size);

  if(commsize > 1){
    snprintf(body, buf_size, "%s", filename_body);
    head = strtok(body, ".");
    post = strtok(NULL, ".");
    snprintf(filename, buf_size, "%s.%d.%s", head, myrank, post);
  } else {
    snprintf(filename, buf_size, "%s", filename_body);
  }
  return filename;
}

/** 並列計算用書き出しファイル名の取得（ローカルコミュニケータから指定） */
const char* monolis_get_local_output_file_name(
  const char* dir_name,
  const char* file_name,
  int         comm)
{
  int commsize;
  int myrank;
  int buf_size = 1000;
  char* head;
  char* post;
  char ctmp[buf_size], body[buf_size];
  char* filename;

  commsize = monolis_get_global_commsize();
  myrank = monolis_get_global_myrank();
  filename = (char*)malloc(sizeof(char)*buf_size);

  if(commsize > 1){
    snprintf(body, buf_size, "%s", filename_body);
    head = strtok(body, ".");
    post = strtok(NULL, ".");
    snprintf(filename, buf_size, "%s.%d.%s", head, myrank, post);
  } else {
    snprintf(filename, buf_size, "%s", filename_body);
  }
  return filename;
}

