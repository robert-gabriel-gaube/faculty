#include <stdio.h>
int main(){
  int x,y;
  scanf("%d",&x);
  if(x!=0){
    do{
      scanf("%d",&y);
      if(y==x*2){
	printf("(%d, %d)\n",x,y);
      }
      x=y; 
    }while(x!=0);
  }
 
  return 0;
}