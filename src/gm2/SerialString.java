package gm2;

import java.math.BigDecimal;
import java.math.BigInteger;


public class SerialString {

	private BigInteger serial;
	
	/**
	 * 
	 * @param serial
	 */
	public SerialString(String serial) {
		super();
		this.serial = new BigInteger(serial).subtract(BigInteger.ONE);
	}	
	
	public int[] algorithm(int m, int n) {
		int[] vet = new int[n];
		BigInteger pow = null;
		BigInteger pow2 = null;
		BigInteger basis = null;
		BigInteger result = null;
		for (int i = 0; i < vet.length; i++) {
			pow = new BigInteger(String.valueOf(m));
			pow2 = new BigInteger(String.valueOf(m));
			basis = new BigInteger(String.valueOf(m));
			result = new BigInteger(String.valueOf(m));
			pow = pow.pow(i+1);
			pow2 = pow.divide(basis);
			result = serial.mod(pow).divide(pow2);
			vet[i] = result.intValue();
			System.out.print(vet[i] + " ");
		}
		System.out.println();
		return vet;
	}
	
	public static void main(String[] args) {
		long time = System.currentTimeMillis();
	//	System.out.println(new BigDecimal("256").pow(250000).toString().length());
		time = System.currentTimeMillis() - time;
		System.out.println("time = "+time);
		int jj = new BigDecimal("2").pow(3).intValue();
		for (int i = 1; i <= jj; i++) {
			SerialString serialString = new SerialString(""+i);
			serialString.algorithm(2,3);	
		}
	}
	

}
