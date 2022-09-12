#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "InstrUtils.h"
#include "Utils.h"

void findInstructionsForRegister(Instruction * ptr, int reg);

void deleteNonCrucialNodes(Instruction * head){

	Instruction * pointer1;//previous instruction
	Instruction * pointer2;//current instruction under consideration
	Instruction * toDelete;//points to instruction that needs to be deleted

	pointer1 = head;

	if(!pointer1) return;
	
	pointer2 = head->prev;

	while(pointer1){

		if(pointer1->critical == 'c'){
			pointer2 = pointer1;
			pointer1 = pointer1->next;
			continue;
		}

		if(pointer1 == head){

			head = pointer1->next;
			if(head){
				head->prev = pointer2;
			}
			toDelete = pointer1;
			pointer1 = pointer1 ->next;
			free(toDelete);
			continue;
		}

		pointer2->next = pointer1->next;

		if(pointer1->next){
			(pointer1 -> next)->prev = pointer2;
		}
		toDelete = pointer1;
		pointer1 = pointer1 -> next;
		free(toDelete);
	}	
	

}

void findInstructionsForCharacter(Instruction * ptr, char character){



	while(ptr){

		if ((ptr->opcode == STORE)&&(ptr->field1 == character)){
		       	ptr->critical = 'c';
	       		findInstructionsForRegister(ptr->prev, ptr->field2);
			break;
	 	}	
		ptr = ptr->prev;	
	}


}

void findInstructionsForRegister(Instruction * ptr, int reg){

	

	while(ptr){

		if (ptr->field1 == reg){
			switch(ptr->opcode){

			case ADD:
			case SUB:
			case MUL:
			case AND:
			case XOR:
				ptr->critical = 'c';
				findInstructionsForRegister(ptr -> prev, ptr->field2);
				findInstructionsForRegister(ptr -> prev, ptr->field3);
				return;
			case LOADI:
				ptr->critical = 'c';
				return;
			case LOAD:
				ptr->critical = 'c';
				char character = ptr->field2;
				findInstructionsForCharacter(ptr -> prev, character);
				return;
			default:
				break;
			}

			
		}	
		ptr = ptr ->prev;
		
	}
}
void findRequiredRegisters(Instruction * ptr, char character){

	//ptr is the pointer to the instruction before the write instruction which writes the character
	//traverse backwards from the pointer until you find a store instruction for this character
	
	while (ptr){

		if((ptr->opcode == STORE) && (ptr->field1 == character)){
			ptr->critical = 'c';
			findInstructionsForRegister(ptr->prev, ptr->field2 );
			break;
		}	

		ptr = ptr->prev;
	}
}

int main()
{
	Instruction *head;

	head = ReadInstructionList(stdin);
	if (!head) {
		WARNING("No instructions\n");
		exit(EXIT_FAILURE);
	}
	/* YOUR CODE GOES HERE */
	
	//Traverse the LL and initialize all char critical fields to 'n' denoting not crucial
	
	Instruction * initializer = head;

	while (initializer){

		initializer -> critical = 'n';
		initializer = initializer -> next;
	}

	//Traverse through the LL and mark all READ and WRITE instructions as critical.
	//For WRITE instructions additionally start a process to determine important instructions.
	
	Instruction * mainPtr = head;

	while(mainPtr){
	
		switch(mainPtr -> opcode){

		case WRITE: 

			mainPtr -> critical = 'c';
			findRequiredRegisters(mainPtr -> prev, mainPtr -> field1);
			break;

		case READ: 

			mainPtr -> critical = 'c';
			break;
		default:
			break;

		}

		mainPtr = mainPtr -> next;

	}	


	deleteNonCrucialNodes(head);
	


	//given instructions

	if (head) {
		PrintInstructionList(stdout, head);
		DestroyInstructionList(head);
	}
	return EXIT_SUCCESS;
}

