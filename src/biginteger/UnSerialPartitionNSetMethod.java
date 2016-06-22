/*
 * Created on 28/09/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package biginteger;

import numbers.BellNumbers;

/**
 * @author glaucio
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class UnSerialPartitionNSetMethod {

private int p[];
private int d[][];
private int n,i,j,k;
private BellNumbers b;

public UnSerialPartitionNSetMethod(int n) {
	this.n = n;
	b = new BellNumbers();
	this.d = fillBuffer();
}

public int unSerialAlgorithm(int[] p) {
	i = j = k = 0;
	int serial = 1;
	int u = 1, v = 0;
	for(i = 1; i < n; i++) {
		for(j = 0; j < p[i]; j++) 
			serial += d[u][v];
		if(j <= p[i-1])
			u++;
		else
			v++;
	}
	return serial;
}

private int[][] fillBuffer() {
	int d[][] = new int[n][];
	for (int i = 0; i < n; i++) {
		d[i] = new int[n - i];
		d[i][0] = (int) b.B(n - i);
	}
	for (int i = 1; i < n; i++) {
		for (int j = 0; j < n-i; j++) {
			d[j][i] = d[j][i - 1] - i * d[j + 1][i - 1];
		}
	}	
	return d;
}

	public static void main(String[] args) {
		int n = 10;
		SerialPartitionNSetMethod test = new SerialPartitionNSetMethod(n);
		UnSerialPartitionNSetMethod test1 = new UnSerialPartitionNSetMethod(n);
		BellNumbers b = new BellNumbers();
		for (int h = 1; h <= b.B(n); h++) {
			System.out.println(test1.unSerialAlgorithm(test.algorithm(h)));
	//		System.out.println(h + " - "+ test);
		}
	//	System.out.println(test1.unSerialAlgorithm(test.algorithm(8)));
	//	System.out.println(test1.unSerialAlgorithm(test.algorithm(9)));	
	
	}
}
