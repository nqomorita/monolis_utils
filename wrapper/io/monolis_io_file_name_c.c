#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "monolis_def_prm_c.h"
#include "monolis_mpi_util_c.h"

/** 並列計算用読み込みファイル名の取得（グローバルコミュニケータから指定） */
const char* monolis_get_global_input_file_name(
  const char* top_dir_name,
  const char* part_dir_name,
  const char* file_name)
{
  int comm_size;
  int my_rank;
  char* filename;

  comm_size = monolis_mpi_get_global_comm_size();
  my_rank = monolis_mpi_get_global_my_rank();
  filename = (char*)malloc(sizeof(char)*MONOLIS_CHARLEN);

  if(comm_size > 1){
    snprintf(filename, MONOLIS_CHARLEN, "%s/%s/%s.%d", top_dir_name, part_dir_name, file_name, my_rank);
  } else {
    snprintf(filename, MONOLIS_CHARLEN, "%s/%s", top_dir_name, file_name);
  }

  return filename;
}

/** 並列計算用読み込みファイル名の取得（ローカルコミュニケータから指定） */
const char* monolis_get_local_input_file_name(
  const char* top_dir_name,
  const char* part_dir_name,
  const char* file_name,
  int         comm)
{
  int comm_size;
  int my_rank;
  char* filename;

  comm_size = monolis_mpi_get_local_comm_size(comm);
  my_rank = monolis_mpi_get_local_my_rank(comm);
  filename = (char*)malloc(sizeof(char)*MONOLIS_CHARLEN);

  if(comm_size > 1){
    snprintf(filename, MONOLIS_CHARLEN, "%s/%s/%s.%d", top_dir_name, part_dir_name, file_name, my_rank);
  } else {
    snprintf(filename, MONOLIS_CHARLEN, "%s/%s", top_dir_name, file_name);
  }

  return filename;
}

/** 並列計算用書き出しファイル名の取得（グローバルコミュニケータから指定） */
const char* monolis_get_global_output_file_name(
  const char* top_dir_name,
  const char* part_dir_name,
  const char* file_name)
{
  int comm_size;
  int my_rank;
  char* filename;

  comm_size = monolis_mpi_get_global_comm_size();
  my_rank = monolis_mpi_get_global_my_rank();
  filename = (char*)malloc(sizeof(char)*MONOLIS_CHARLEN);

  if(comm_size > 1){
    snprintf(filename, MONOLIS_CHARLEN, "%s/%s/%s.%d", top_dir_name, part_dir_name, file_name, my_rank);
  } else {
    snprintf(filename, MONOLIS_CHARLEN, "%s/%s", top_dir_name, file_name);
  }

  return filename;
}

/** 並列計算用書き出しファイル名の取得（ローカルコミュニケータから指定） */
const char* monolis_get_local_output_file_name(
  const char* top_dir_name,
  const char* part_dir_name,
  const char* file_name,
  int         comm)
{
  int comm_size;
  int my_rank;
  char* filename;

  comm_size = monolis_mpi_get_local_comm_size(comm);
  my_rank = monolis_mpi_get_local_my_rank(comm);
  filename = (char*)malloc(sizeof(char)*MONOLIS_CHARLEN);

  if(comm_size > 1){
    snprintf(filename, MONOLIS_CHARLEN, "%s/%s/%s.%d", top_dir_name, part_dir_name, file_name, my_rank);
  } else {
    snprintf(filename, MONOLIS_CHARLEN, "%s/%s", top_dir_name, file_name);
  }

  return filename;
}

/** 並列計算用書き出しファイル名の取得（ローカルコミュニケータから指定） */
const char* monolis_get_output_file_name_by_domain_id(
  const char* top_dir_name,
  const char* part_dir_name,
  const char* file_name,
  int         domain_id)
{
  char* filename;

  filename = (char*)malloc(sizeof(char)*MONOLIS_CHARLEN);

  snprintf(filename, MONOLIS_CHARLEN, "%s/%s/%s.%d", top_dir_name, part_dir_name, file_name, domain_id);

  return filename;
}