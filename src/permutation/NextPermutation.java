package permutation;

import exceptions.*;
import java.util.Date;

/**
 * Esta classe oferece alguns metodos para problemas de permutacao.
 * @author Glaucio Melo.
 */
public class NextPermutation {

	// Campos da classe NextPermutation.
	private int j;
	private int r;
	private int[] a;
	private int[] d;
	private int n;
	private java.lang.String str;
	private boolean flag;

	/**
	 * @param n int Tamanho do array
	 * @param s int Parâmetro auxiliar no retorno Este método retorna o
	 * factorial de n.
	 */
	private int factorial(int n, int s) {
		for (j = 2; j <= n; j++)
			s *= j;
		return s;
	}
	
	/**
	 * Código para executar quando esse objeto for coletado do lixo.
	 * Qualquer exceção lançada por um método de finalização faz com que a finalização seja
	 * interrompida. Caso contrário, ela é ignorada.
	 */
	protected void finalize() throws Throwable {
		flush();
	}

	/**
	 * @param i int Atributo de comunicação com o método nextper
	 * @param x int Parâmetro auxiliar no retorno
	 * @param w int Parâmetro auxiliar no retorno Este método retorna o índice
	 * de uma busca correspondente do menor elemento maior do que x (a[i+1]).
	 */
	private int searchForOdd(int i, int x, int w) {
		for (j = 0; j <= i; j++)
			if ((a[j] < a[i + 1]) && (a[j] > x)) {
				x = a[j];
				w = j;
			}
		return w;
	}
	
	/**
	 * @param i int Atributo de comunicação com o método nextper
	 * @param x int Parâmetro auxiliar no retorno
	 * @param w int Parâmetro auxiliar no retorno Este método retorna o indice
	 * correspondente a busca do Maior número menor que x (a[i+1]).
	 */
	private int searchForEven(int i, int x, int w) {
		for (j = 0; j <= i; j++)
			if ((a[j] > a[i + 1]) && (a[j] < x)) {
				x = a[j];
				w = j;
			}
		return w;
	}
	
	/**
	 * @return int A soma dos elementos de d0 até di.
	 * @param i int Atributo de comunicação com o método nextper.
	 * @param s int Parâmetro auxiliar no retorno
	 */
	private int sum(int i, int s) {
		for (j = 0; j <= i; j++)
			s += d[j];
		return s;
	}
	
	/**
	 * Metodo que retorna verdadeira caso o numero seja impar.
	 * @return boolean
	 * @param j int Número a ser analisado no método Este método retorna false
	 * se um número for par; caso contrário, o método retorna ímpar;
	 */
	private boolean odd(int j) {
		if (j % 2 == 1)
			return true;
		return false;
	}

	/**
	 * Este método troca dois elementos do Array a[], E incrementa o serial.
	 */
	private void swap(int a[], int k, int f, boolean t) {
		j = a[k];
		a[k] = a[f];
		flag = t;
		a[f] = j;
		r++;
	}
	
	/**
	 * @param i int Índice de comunicação com o método nextper.
	 * @param n int Tamanho do array; Este método atualiza a troca se caso a
	 * soma do array d seja par;
	 */
	private void sumEven(int i, int n) {
		swap(a, i + 1, searchForEven(i, n, 0), false);
		d[i] = Math.abs(d[i] - 1);
	}
	
	/**
	 * @param i int Índice de comunicação com o método nextper. Este método
	 * atualiza a troca se caso a soma do array d seja impar;
	 */
	private void sumOdd(int i) {
		swap(a, i + 1, searchForOdd(i, -1, 0), false);
		d[i]++;
	}
	
	/**
	 * Este método atualiza a troca se caso o serial seja PAR;
	 */
	private void sumNeuter() {
		swap(a, 0, 1, true);
		d[0] = Math.abs(d[0] - 1);
	}
	
	/**
	 * Este metodo inicializa os atributos;
	 */
 	private void initialize(int n) throws OutOfRangeException {
	 if (n < 2)
	  throw new OutOfRangeException();
	 else {
	   str = "";
	   j = r = 0;
	   a = new int[n];
	   d = new int[n - 1];
	   flag = false;
	   for (j = 0; j < n; j++)
		a[j] = j;
	}
	}
	
	/**
	 * Calcula a proxima permutacao
	 * @param i indice de recursao do metodo.
	 * @return int[] vetor de permutacao
	 * @throws OutOfRangeException Excecao de limites.
	 */
	private int[] nextper(int i) throws OutOfRangeException {
	/* if (r >= factorial(n, 1) - 1)
	  throw new OutOfRangeException();
	 else {*/
	  if (!flag)
	   sumNeuter();
	  else
	   if ((!odd(sum(i, 0)) || (d[i] > i)) && (odd(sum(i, 0)) || (d[i] <= 0)))
	    return nextper(i + 1);
	   else
		if (odd(sum(i, 0)))
	     sumOdd(i);
		else
	     sumEven(i, n);
	 return a;
	// }
	}

	/**
	 * Construtor da classe NextPermutation.
	 * @param n Tamanho do vetor de permutacao.
	 */
	public NextPermutation(int n) {
		this.n = n;
		initialize();
	}

	/**
	 * Invoca o Garbage Colector.
	 */
	public void flush() {
		a = d = null;
		System.gc();
	}

	/**
	 * Retorna a permutacao atual.
	 * @return int[] Vetor de permutacao.
	 */
	public int[] actual() {
		return a;
	} 
	
	/**
	 * Atualiza o vetor de permutacao a partir de um serial especifico.
	 * @param num indice da permutacao.
	 * @param n Tamanho do vetor de permutacao.
	 * @exception OutOfRangeException Indice fora dos limites.
	  */  
	private int[] especifiedPermutation(int num)  throws OutOfRangeException {
	 if ((num < 1) || (num > factorial(n, 1)))
	  throw new OutOfRangeException();
	 else {
	  initialize(n);
	  while (r != num - 1)
	   nextper();
	}
	 return a;
	}
	
	/**
	* Calcula o fatorial de n.
	* @return fatorial do numero.
	*/
	public int getFactorial() {
		return factorial(n, 1);
	} 
	
	/**
	 * Retorna a proxima permutacao.
	 * @return int[] Vetor de permutacao.
	 */
	public int[] nextper() {
	 try {
	  return nextper(1);
	 } catch (OutOfRangeException error) {
		System.err.println(error);
		System.exit(1);
	   }
	return null;
	} 
	
	/**
	 * Retorna um Strong Padrao no formato (a, b, c)
	 * @return Vetor convertido em um String.
	 */
	public String outPut(int[] a) {
	 return outPut(a, "(", ",", ")");
	} 
	
	/**
	 * Retorna o formato de saida generalizado.
	 * @return Vetor convertido em um String.
	 */
	public String outPut(int[] a, String open, String space, String close) {
	 StringBuffer str = new StringBuffer();
		str.append((r + 1) + " -> " + open);
		for (j = 0; j < n - 1; j++)
			str.append((a[j] + 1) + space);
		str.append((a[n - 1] + 1) + close);
		str.append(" "+open + d[0] + space);
		for (j = 1; j < n-2; j++)
		 str.append(d[j] + space);
		str.append(d[n-2] + close);
		return str.toString();
	} 
	
	/**
	 * Retorna a permutacao Serial.
	 * @return Serial vetor de permutacao indexado pelo serial. 
	 */
	public int[] serialNextper(int num) {
	 try {
	  return especifiedPermutation(num);
	 } catch (OutOfRangeException error) {
		System.err.println(error);
		System.exit(1);
	   }
	return null;
	}

	/** 
 	 * Instancia das variaveis.
 	 */
	public void initialize() {
	 try {
	  initialize(n);
	 } catch (OutOfRangeException error) {
		System.err.println(error);
		System.exit(1);
	   }
	}

	/**
 	 * Metodo main (para testes).
 	 * @param args String de entrada.
 	 */
	public static void main(String[] args) {
	 long t1,t0;
	 Date r;
	 r = new Date();
	 t0 = r.getTime();
	 int n = 5;
	 NextPermutation number = new NextPermutation(n);
	 int[] vec = number.actual();
	 System.out.println(number.outPut(vec));
	 for(int i = 0; i < number.getFactorial()-1; i++) {
	  vec = number.nextper();
	  System.out.println(number.outPut(vec));
	 }
	 r = new Date();
	 t1 = r.getTime() - t0; 
	 System.out.println("tempo: "+ t1);	
	}
}