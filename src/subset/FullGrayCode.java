package subset;

/** 
 * Classe que implementa Gray Code.
 * @author Glaucio Melo.
 */
public class FullGrayCode {

	// Campos da Classe Gray
	private int i,j,g[];
	private String k1;
	private int n;
	
	/**
	 * Construtor para a classe Gray.
	 * @param n numero de bits.
	 */
	public FullGrayCode(int n) {
	 this.n = n;
	 g = new int[p(n)];
	 gray(this.n);
	}
	
	// Metodo nucleo da classe Gray.
	private void gray(int n) {
	 g[0]=0;
	 g[1]=1;
	 for(j=2;j<=n;j++)
	  for(i=p(j-1);i<=p(j)-1;i++) 
	   if (i-p(j-1)<=p(j-2)-1)
		g[i]=g[i-p(j-1)]+p(j-1)+p(j-2);
	   else
		g[i]=g[i-p(j-1)]+p(j-2);	
	}
	
	// Potencia em base 2.
	private  int p(int b) {
	 return (int) Math.pow(2,(int) b);
	}

	/**
	 * Captura em forma de String a saida da sequencia, em decimal.
	 * @return String Saida em Decimal do codigo Gray.
	 */
	public String getIntegerOutPut() {
	 StringBuffer k1 = new StringBuffer();
	 for(j=0;j<p(n);j++)
	  k1.append(g[j] + " ");
	return k1.toString();
	}
	
	/**
	 * Captura em forma de String a saida da sequencia, em binario.
	 * @return String Saida em Binario do Codigo Gray.
	 */
	public String getBinaryOutPut() {
	 StringBuffer k1 = new StringBuffer();
	 for(i=0;i < p(n); i++) {
	  k1.append((i+1)+ "- ");
	  for(j=p(n-1);j>0;j=j/2)
	   k1.append(((g[i] & j)==0 ? "0 ":"1 "));
	  k1.append("\n");
	 }
	return k1.toString();
	}
	
	public static void main(String[] args) {
		FullGrayCode g = new FullGrayCode(5);
		System.out.println(g.getIntegerOutPut());
		System.out.println(g.getBinaryOutPut());
	}
}
