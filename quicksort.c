#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

// for swapping values in array
void swap(int *x, int *y)
{
  // intermediate place holder variable
  int z;

  // allocate value of x to z, set x to y and y to z
  z = *x;
  *x = *y;
  *y = z;
  return ;
}

// for swapping values in array
void fswap(float *x, float *y)
{
  // intermediate place holder variable
  float z;

  // allocate value of x to z, set x to y and y to z
  z = *x;
  *x = *y;
  *y = z;
  return ;
}

// prints out int list
void display(int a[]) {
  int i;
  int n=0;
  int p = -1;
  while (p!= '\0')
  {
    n++;
    p = a[n];
  }

  printf("[");

  for(i = 0;i<n;i++) {
    printf("%d ",a[i]);
  }

  printf("]\n");
}

// prints out float list
void fdisplay(float a[]) {
  int i;
  int n=0;
  int p = -1;
  while (p!= '\0')
  {
    n++;
    p = a[n];
  }

  printf("[");

  for(i = 0;i<n;i++) {
    printf("%f ",a[i]);
  }

  printf("]\n");
}

// Function that uses median of three partitioning
int median_of_3(int a[], int left, int right)
{
  // get middle of array
  int middle = (left + right)/2;

  // rearrange arrange now with pivot known
  if (a[middle] < a[left]) {
    swap(&a[left],&a[middle]);
  }
  if (a[right] < a[left]) {
    swap(&a[left],&a[right]);
  }
  if (a[right]< a[middle]) {
    swap(&a[middle],&a[right]);
  }

  swap(&a[middle], &a[right - 1]);
  return a[right - 1];
}

float fmedian_of_3(float a[], int left, int right)
{
  // get middle of array
  int middle = (left + right)/2;

  // rearrange arrange now with pivot known
  if (a[middle] < a[left]) {
    fswap(&a[left],&a[middle]);
  }
  if (a[right] < a[left]) {
    fswap(&a[left],&a[right]);
  }
  if (a[right] < a[middle]) {
    fswap(&a[middle],&a[right]);
  }

  fswap(&a[middle], &a[right - 1]);
  return a[right - 1];
}

// Function that uses median of three partitioning
int show_median_of_3(int a[], int left, int right)
{
  // get middle of array
  int middle = (left + right)/2;
  printf("============================================================\n");
  printf("At this step:\n");
  printf("current array:");
  display(a);
  printf("left value: %d\n", a[left]);
  printf("right value: %d\n", a[right]);
  printf("middle value: %d\n", a[middle]);

  // rearrange arrange now with pivot known
  if (a[middle] < a[left]) {
    swap(&a[left],&a[middle]);
    printf("pivot swapped: %d,%d\n", a[middle],a[left]);
    display(a);
  }
  if (a[right] < a[left]) {
    swap(&a[left],&a[right]);
    printf("pivot swapped: %d,%d\n", a[right],a[left]);
    display(a);
  }
  if (a[right]< a[middle]) {
    swap(&a[middle],&a[right]);
    printf("pivot swapped: %d,%d\n", a[right],a[middle]);
    display(a);
  }

  swap(&a[middle], &a[right - 1]);
  return a[right - 1];
}

float show_fmedian_of_3(float a[], int left, int right)
{
  // get middle of array
  int middle = (left + right)/2;
  printf("============================================================\n");
  printf("At this step:\n");
  printf("current array:");
  fdisplay(a);
  printf("left value: %f\n", a[left]);
  printf("right value: %f\n", a[right]);
  printf("middle value: %f\n", a[middle]);

  // rearrange arrange now with pivot known
  if (a[middle] < a[left]) {
    fswap(&a[left],&a[middle]);
    printf("pivot swapped:%f,%f\n", a[middle],a[left]);
    fdisplay(a);
  }
  if (a[right] < a[left]) {
    fswap(&a[left],&a[right]);
    printf("pivot swapped:%f,%f\n", a[right],a[left]);
    fdisplay(a);
  }
  if (a[right] < a[middle]) {
    fswap(&a[middle],&a[right]);
    printf("pivot swapped:%f,%f\n", a[right],a[middle]);
    fdisplay(a);
  }

  fswap(&a[middle], &a[right - 1]);
  return a[right - 1];
}

void showQuickSort(int a[], int left, int right)
{
  if (left < right) {
    int pivot = show_median_of_3(a,left,right);
    if (left == right - 1) return;
    int i = left;
    int j = right - 1;
    for ( ; ;) {
      while(a[++i]<pivot) {}
      while(pivot<a[--j]) {}
      if ( i < j) {
        swap(&a[i],&a[j]);
        printf("pivot is %d, i is at index %d and j is at index %d\n", pivot, i, j);
        printf("numbers swapped :%d,%d\n", a[i],a[j]);
        display(a);
      } else {
        break ;
      }
    }
    swap(&a[i],& a[right -1]);
    showQuickSort(a,left,i-1);
    showQuickSort(a,i+1,right);
  }

  return ;
}

void quickSort(int a[], int left, int right)
{
  if (left < right) {
    int pivot = median_of_3(a,left,right);
    if (left == right - 1) return;
    int i = left;
    int j = right - 1;
    for ( ; ;) {
      while(a[++i]<pivot) {}
      while(pivot<a[--j]) {}
      if ( i < j) {
        swap(&a[i],&a[j]);
      } else {
        break ;
      }
    }
    swap(&a[i],& a[right -1]);
    quickSort(a,left,i-1);
    quickSort(a,i+1,right);
  }

  return ;
}

void fquickSort(float a[], int left, int right)
{
  if (left < right) {
    int pivot = fmedian_of_3(a,left,right);

    if (left == right - 1) return;
    int i = left;
    int j = right - 1;
    for ( ; ;) {
      while(a[++i]<pivot) {}
      while(pivot<a[--j]) {}
      if ( i < j) {
        fswap(&a[i],&a[j]);
      }
      else {
        break ;
      }
    }
    fswap(&a[i],& a[right -1]);
    fquickSort(a,left,i-1);
    fquickSort(a,i+1,right);
  }

  return ;
}


void showfQuickSort(float a[], int left, int right)
{
  if (left < right) {
    int pivot = show_fmedian_of_3(a,left,right);

    if (left == right - 1) return;
    int i = left;
    int j = right - 1;
    for ( ; ;) {
      while(a[++i]<pivot) {}
      while(pivot<a[--j]) {}
      if ( i < j) {
        fswap(&a[i],&a[j]);
        printf("pivot is %d, i is at index %d and j is at index %d\n", pivot, i, j);
        printf("numbers swapped :%f,%f\n", a[i],a[j]);
        fdisplay(a);
      }
      else {
        break ;
      }
    }
    fswap(&a[i],& a[right -1]);
    showfQuickSort(a,left,i-1);
    showfQuickSort(a,i+1,right);
  }

  return ;
}

// int main()
// {
//   int a[] = {10, 100, 30, 90, 40, 50, 7};
//   quickSort(a, 0, 6);
//   showQuickSort(a, 0, 6);
//
//
//   int b[] = {85,331,234,46,4,3,22,89,7,12, 33, 44,55,66,77};
//   showQuickSort(b, 0, 14);
//   quickSort(b,0,14);
//
//
//   float c[] = { 3.4, 56.2, 5.3, 6.0, 2.6, 566.7, 778.3};
//   showfQuickSort(c, 0, 6);
//   fquickSort(c, 0, 6);
// }
