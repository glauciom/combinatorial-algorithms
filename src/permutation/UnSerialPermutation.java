/*
 * Created on 05/04/2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package permutation;

/**
 * @author glaucio
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class UnSerialPermutation {

	private int n;
	private int[] permutation;
	private int[] offset;
	private int[] fatorial;
	private boolean normal, even;

	/**
	 * Construtor para UnSerialPermutation.
	 * @param permutation vetor de permutação (entrada de permutação). 
	 */
	public UnSerialPermutation(int[] permutation) {
		this.permutation = permutation;
		this.n = permutation.length;
		this.normal = true;
		offset = new int[n-1];
		fatorial = new int[n-1];
		init();
		
	}
	
	
	private void init() {
		fatorial[0] = 1;
		for(int i = 1; i < fatorial.length; i++) {
		 fatorial[i] = fatorial[i-1] * (i+1);
		}
	}
	private int[] getOffset() {
		for(int i = 0; i < offset.length; i++)
		 for(int j = 0; j <= i; j++)
		  if(permutation[j] > permutation[i+1])
		   offset[i]++;
    return offset;
	}
	
	public int unSerialPermutationAlgorithm() {
		offset = getOffset();
		int serial = offset[offset.length -1] * fatorial[fatorial.length -1] + 1;
		for(int i = offset.length-1; i > 0; i--) {
			if((isEven(offset[i]) && normal) || (!isEven(offset[i]) && !normal)) { //Lista Normal
			  normal = true;
			  serial += offset[i-1] * fatorial[i-1];
			} else 
			   if((!isEven(offset[i]) && normal) || (isEven(offset[i]) && !normal)) {
			   	normal = false;
			   	serial += (i - offset[i-1]) * fatorial[i-1]; 
			   }
		}
		System.out.println(serial);
	return serial;
	}
	
	

	/**
	 * Método main (para testes) 
	 * @param args Parâmetros de entrada.
	 */
	public static void main(String[] args) {
		NextPermutation number = new NextPermutation(4);
			 int[] vec = number.actual();
		    UnSerialPermutation test; 
			 test = new UnSerialPermutation(vec);
			 test.unSerialPermutationAlgorithm();
			 for(int i = 0; i < number.getFactorial()-1; i++) {
			  vec = number.nextper();
			  test = new UnSerialPermutation(vec);
			  test.unSerialPermutationAlgorithm();
			 }

	}

	/**
	 * @return
	 */
	public boolean isEven(int k) {
		return k % 2 == 0;
	}

	/**
	 * @return
	 */
	public boolean isNormal() {
		return normal;
	}

	/**
	 * @param b
	 */
	public void setEven(boolean b) {
		even = b;
	}

	/**
	 * @param b
	 */
	public void setNormal(boolean b) {
		normal = b;
	}

}
