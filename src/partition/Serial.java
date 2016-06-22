/*
 * Created on 28/08/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package partition;

import numbers.BellNumbers;

/**
 * @author GLAUCIO
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class Serial {

	private long serial;
	private BellNumbers B;
	private int p[], i,j,n;
	long z = 0;
	long s = 0;
	long tmp = 0;
	
	public Serial(int n) {
		this.n = n;
		p = new int[n];
	}
	
	public int[] serialPartitionNSetAlgorithm(long serial) {
	/*	this.serial = serial;
		B = new BellNumbers();
		for(i = 1; i < n; i++) 
			p[i] = getElement(serial);
		return p;*/
		this.serial = serial;
		B = new BellNumbers();
		for (int i = 1; i < n-1; i++) {
			p[i] = element(i,serial);
		}
		return p;
	}
	
	public int element(int i, long s) {
		int val = (int) (((s-1) % B.B(n-i))/B.B(n-i-1));		
		if (val > i) {
			return i;
		}
		return val;
	}
	
	private int getElement(long serial) {
		for(j = 0; j < i; j++) {
			tmp = s + B.B(n-(p[i-1]+1)); 
			if(tmp <= serial) {
				s = tmp;
			} else {
				tmp = s;
				return j+1;
			}
			
		}	
		return i;
	}
	
	
	
	
	public static void main(String[] args) {
		// 150-> 0 1 2 1 1 2 
		Serial teste = new Serial(5);
	//	for (int i = 1; i <= 52; i++) {
			int[] p = teste.serialPartitionNSetAlgorithm(52);
			//System.out.print(i+"- ");
			for (int j = 0; j < p.length; j++) {
				System.out.print(p[j] + " ");
			}
			System.out.println();
		}
	//}
}
