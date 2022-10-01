package limbo.leetcodeSolution

import scala.collection.mutable.ArrayBuffer

object BackPack {
  // 01背包问题
  /*
     有 N 件物品和一个容量是 V 的背包。每件物品只能使用一次。

     第 i 件物品的体积是 vi，价值是 wi。
                                               ***
     求解将哪些物品装入背包，可使这些物品的总体积不超过背包容量，且总价值最大。
     输出最大价值。

     输入格式
     第一行两个整数，N，V，用空格隔开，分别表示物品数量和背包容积。

     接下来有 N 行，每行两个整数 vi,wi，用空格隔开，分别表示第 i 件物品的体积和价值。

     输出格式
     输出一个整数，表示最大价值。

     数据范围
     0<N,V≤1000
     0<vi,wi≤1000
     输入样例
     4 5
     1 2
     2 4
     3 4
     4 5
     输出样例：
     8

     f[i][j] 表示只看前i个物品，总体积是j的情况下，最大价值是多少

     result = max[f[n][0 ~ v]]

     f[i][j]:

     1.不选第i个物品： f[i][j] = f[i - 1][j]
     2.选第i个物品: f[i][j] = f[i - 1][j - v[i]] + w[i]

     f[i][j] = max{1,2}

     f[0][0] = 0 一个物品都不考虑，没有占用体积

     O(n^2) θ(N * V)

     优化:
     所有的f[i] 只与 f[i - 1] 有关

     体积小于 x 的情况下最大价值是 f(x)
     若初始化为全0，f(num)直接得出结果

     若第一个元素初始化0，其余元素无穷小
     结果是f[num]恰好是最优

     f[0] = 0
     f[i] = 0

     假设 k 体积下是最优解，且 k < (m = volume)
     f[k] = max_w 小于等于k体积情况下的最高价值

     f[0] = 0 => f[v[0]] = w[0] => ...
     f[m - k] = 0 => f[m - k + v[0]] = w[0] => ...

     对j从大到小遍历，就可以保证 f[j] = f[j - v[i]] + w[i]
     中的 f[j - v[i]] 是未优化作法中的f[i - 1]中的状态
     由于变量赋值前后的关系 f[j] = math.max(f[j],f[j - v[i]] + w[i])
     max内的f[j]是上一次的，也就是 f[i - 1][j - v[i]]

     最终结果就是 f[volume]

     若要求体积恰好是volume的情况下
     f[0] = 0
     f[i] = -INF
     确保了所有状态都是从f(0)转移



  */
  def zeroOneBackPack(N: Int, V: Int, ele: Array[Array[Int]]): Int = {
    /*
    val f = Array.ofDim[Int](num + 1,volume + 1)
    f(0)(0) = 0

    for(i <- 1 to num){
      for(j <- 0 to volume){
        val vi = ele(i)(0)
        val wi = ele(i)(1)

        // don't choose ith ele
        f(i)(j) = f(i - 1)(j)
        // other elements can fill in j - vi
        // >= in case of i = 1
        // i.e f[1][j] = f(0)(j - v[1]) + w[1]
        // other elements can fill in the remain volume
        // i.e there is a situation that f(i - 1)(j - vi) exists
        if(j - vi >= 0)
          f(i)(j) = f(i - 1)(j - vi) + wi
      }
    }

    f(num).max
    */

    val f = Array.fill(N + 1)(0)
    for (i <- 1 to N) {
      val vi = ele(i)(0)
      val wi = ele(i)(1)
      //for(j <- V to vi by -1)+
      for (j <- V to 0 by -1 if j - vi >= 0)
        f(j) = math.max(f(j - vi) + wi, f(j))
    }

    f(N)
  }


  //完全背包问题
  /*
                                    ***************
    有 N 种物品和一个容量是 V 的背包，每种物品都有无限件可用。

    第 i 种物品的体积是 vi，价值是 wi。

    求解将哪些物品装入背包，可使这些物品的总体积不超过背包容量，且总价值最大。
    输出最大价值。

    输入格式
    第一行两个整数，N，V，用空格隔开，分别表示物品种数和背包容积。

    接下来有 N 行，每行两个整数 vi,wi，用空格隔开，分别表示第 i 种物品的体积和价值。

    输出格式
    输出一个整数，表示最大价值。

    数据范围
    0<N,V≤1000
    0<vi,wi≤1000
    输入样例
    4 5
    1 2
    2 4
    3 4
    4 5
    输出样例：
    10

    f[i] 表示总体积是i的情况下，最大价值是多少

    res = max{f[0] ... f[N]}

    for(int i = 0; i < m ; i++){
      for(int j = v[i] ; j <= m ; j++)
        f[j] = max(f[j],f[j - v[i]] + w[i])

      or

      for(int j = m; j >= v[i]; j--){
        for(int k = 0; k * v[i] <= j; k++)
          //选第i个物品且选k次，直到达到体积j
          f[j] = max(f[j] , f[j - k * v[i]] + k * w[i])

      }

    }



    数学归纳法证明由小到大遍历j可以包含选多次的情况:
    1.假设考虑前 i - 1个物品后，所有f[j]都是正确的
    2.需要证明，考虑完前i个物品之后，所有的f[j]也都是正确的

    对于某个j而言，如果最优解中包含k个v[i]

    a.f[j - k * v[i]] 先计算到
    且会用到 f[j - k * v [i] - v[i]] + w[i] 来更新
    但既然 f[j - k * v[i]]已经被假设1确定
    意味着 f[j - k * v[i] - v[i]] + w[i] 已确定

    a => b.f[j - (k - 1) * v[i] - v[i]] + w[i]
    需要使用 a 来更新
    b会包含一个v[i]

    ...

    f[j]
    c.f[j - v[i]] + w[i]
    f[j - v[i]] 一定包含 k - 1 个 v[i] 的情况，与此同时又 + w[i]
    c一定包含了k个v[i]的情况
    证毕

  */
  def fullBackPack(N: Int, V: Int, ele: Array[Array[Int]]): Int = {
    // init all 0 leads to best solution which volume less or equal to V
    // if we need the best solution that volume exactly equal to V
    // init f[0] = 0 , other -INF
    val f = Array.fill(N + 1)(0)
    for (i <- 1 to N) {
      val vi = ele(i)(0)
      val wi = ele(i)(1)
      for (j <- vi to V)
        f(j) = math.max(f(j - vi) + wi, f(j))
    }

    f(V)
  }


  //多重背包问题
  /*
    有 N 种物品和一个容量是 V 的背包。

    第 i 种物品最多有 si 件，每件体积是 vi，价值是 wi。

    求解将哪些物品装入背包，可使物品体积总和不超过背包容量，且价值总和最大。
    输出最大价值。

    输入格式
    第一行两个整数，N，V，用空格隔开，分别表示物品种数和背包容积。

    接下来有 N 行，每行三个整数 vi,wi,si，用空格隔开，分别表示第 i 种物品的体积、价值和数量。

    输出格式
    输出一个整数，表示最大价值。

    数据范围
    0<N,V≤100
    0<vi,wi,si≤100

    ---------------------------
    暴力:
    f[i] 总体积是i的情况下，最大价值是多少

    for(int i = 0; i < n ;i++){
      for(int j = m; j >= v[i]; j--)
        f[j] = max(f[j],f[j - v[i]] + w[i],f[j - 2 * v[i]] + 2 * w[i] ....)

    }

    1.f[i] = 0
    f[m]

    2.f[0] = 0,f[i] = -INF i != 0
    max{f[0 ... m]}

    01背包的扩展，从选或者不选过渡到当前最多可以选si次

    ---------------------------
    二进制：如果s的上限很大
    第i件物品换成若干件物品
    使得原问题中第i种物品可取的每种策
    从取0..n[i]件 ==> 均能等价于取若干件代换以后的物品。
    另外，取超过n[i]件的策略必不能出现

    将第i种物品分成若干件物品，其中每件物品有一个系数，
    这件物品的费用和价值均是原来的费用和价值乘以这个系数。
    使这些系数分别为 1,2,4,...,2^(k-1),n[i]-2^k+1，且k是满足n[i]-2^k+1>0的最大整数。
    例如，如果n[i]为13，就将这种物品分成系数分别为1,2,4,6的四件物品。

    n[i] - 2为比的数列前n项和 尽可能趋向 0

    EX:
    13 - 2^3 + 1 = 6 > 0
    13
    1 2 4 6
    4个数可以表示从 1 ~ 13所有情况

    外层element,内层j,且j 从大到小遍历,f(j - e.v) 先算
    EX.
    f(5) = max(f(5),f(5 - 1) + e1.w)
    f(5) = max(f(5),f(5 - 2) + e2.w)
    f(5) = max(f(5),f(5 - 4) + e4.w)
    f(4) = max(f(4),f(4 - 1) + e1.w)
    f(4) = max(f(4),f(4 - 2) + e2.w)
    f(4) = max(f(4),f(4 - 4) + e4.w)
      ...

    观察f(5)
    f(4) = max(f(4),f(4 - 2) + e2.w)
    f(5) = max(f(5),f(5 - 1) + e1.w) =
    max(f(5),max(f(4),f(2) + e2.w) + e1.w)

    从定义上考虑，如果需要考虑，或者说状态转移到了
    f(5) = max(f(5),f(2) + e2.w) = max(f(5),f(5 - 3) + e2.w)
    f(4) 必然是 f(2) + e2.w
    表达完整性得证

    // the residual and the sum can represent all the 1 ~ si numbers
    for(e <- elements){
      for(j <- V to 0 by -1 if j >= e.v)
        f(j) = Math.max(f(j),f(j - e.v) + e.w)
    }

    分成的这几件物品的系数和为n[i]，表明不可能取多于n[i]件的第i种物品。
    另外这种方法也能保证对于0..n[i]间的每一个整数，均可以用若干个系数的和表示，
    这个证明可以分0..2^k-1和2^k..n[i]两段来分别讨论得出

    将第i种物品分成了O(log n[i])种物品，将原问题转化为了复杂度为O(V*∑log n[i])的01背包问题

    v,w

    s = 10
    1 2 4 最多可以组合到7

    1 ~ 7
    +3
    1 ~ 10


    ----------------------
    单调队列求滑动窗口最大 O(N * M)

    #include<bits/stdc++.h>
    using namespace std;
    const int maxn=110;
    int n,m;
    int v[maxn],w[maxn],s[maxn],f[maxn][maxn];
    int main()
    {
        cin>>n>>m;
        for(int i=1;i<=n;i++) cin>>v[i]>>w[i]>>s[i];
        for(int i=1;i<=n;i++)
        {
            //在 0 ~ m 的空间范围尝试第i种物品
            for(int j=m;j>=0;j--)
            {
                for(int k=0;k<=s[i] && j-k*v[i]>=0;k++)
                {
                    f[i][j]=max(f[i-1][j],f[i-1][j-k*v[i]]+k*w[i]);
                }
            }
        }
        cout<<f[m]<<endl;
    }

    仔细观察，对于任意的j，都可以从v[i]的倍数转移过来。

    f[j] = max(f[j - v] + w, f[j - 2v] + 2w , ..., f[j - s * v] + s * w)
    f[j + v] = max(f[j] , f[j - 2v] + 2w , ... , f[j - (s - 1) * v] + (s - 1) * w)
      ...

    f[k*v+j] 只依赖于 { f[j], f[v+j], f[2*v+j], f[3*v+j], ... , f[k*v+j] }

    在无优化的时候却每次把所有的倍数都遍历了一遍。

    因为我们需要的是{ f[j], f[v+j], f[2*v+j], f[3*v+j], ... , f[k*v+j] } 中的最大值，
    可以通过维护一个单调队列来得到结果。
    且按余数分j组，各组之间互不影响，这样的话，问题就变成了 j 个单调队列依次求滑动窗口最大的问题

    可以把0 ~ m的空间范围
    根据模v[i]的余数分为 v[i] 个子集。

    for(int j=0;j<v;j++) 此处v为第i件物品所占空间
    j 从0 ~ v - 1 遍历意味着 考虑第i件物品的时候，
    从 0 ~ v - 1 这个余数范围开始，逐步加上v的倍数
    从而映射到 0 ~ m 的 v[i] 个子集

    for(int k=j;k<=m;k+=v)的定义保证窗口整体移动不会越界
    此时的k相当于原来的j，但是我们可以利用k和v之间存在的倍数关系去做滑动窗口。
    要根据当前的k判断窗口里存在的k对应的值包含了多少个v，以便于计算新的价值
    
    v的个数=(当前空间 - 余数)/v   价值 =(当前空间 - 余数)/v * w
           =(k - j)/v                =(k - j) / v * w
    每次只用了i-1的值，可以优化一下空间

    从v[i]的余数范围开始，考虑每一个可能的体积k - s * v[i] 到 k 的窗口

    f[j]    =     f[j]
    f[j+v]  = max(f[j] +  w,  f[j+v])
    f[j+2v] = max(f[j] + 2w,  f[j+v] +  w, f[j+2v])
    f[j+3v] = max(f[j] + 3w,  f[j+v] + 2w, f[j+2v] + w, f[j+3v])
    ...
    但是，这个队列中前面的数，每次都会增加一个 w ，在比较价值的时候会造成麻烦，
    所以我们需要做一些转换，函数形参均减去当前倍数的w，且在后面加回来
    f[j]    =     f[j]
    f[j+v]  = max(f[j], f[j+v] - w) + w
    f[j+2v] = max(f[j], f[j+v] - w, f[j+2v] - 2w) + 2w
    f[j+3v] = max(f[j], f[j+v] - w, f[j+2v] - 2w, f[j+3v] - 3w) + 3w
    ...

    例:
    f[j + 2v] - 2w = max(f[j] , max(f[j],f[j + v] - w) , f[j + 2v] - 2w)
                   = max(f[j], f[j + v] - w , f[j + 2v] - 2w)
    同理:
    f[j + kv] - kw = max(f[j] , f[j + v] - w, f[j + 2v] - 2w , ... , f[j + kv] - kw) , k <= s

    滑动窗口内的比较可以转换为比较 f[j + k * v] - k * w

    用单调队列维护窗口内的最优选择,单调队列要维护一个不超过s的集合
    窗口整体推进的过程中同步更新最优选择

    #include <bits/stdc++.h>
    using namespace std;

    int n, m;
    int f[20002], q[20002], g[20002];
    int main() {
        cin >> n >> m;
        for (int i = 0; i <= n; ++i) {
            int v, w, s;
            cin >> v >> w >> s;
            memcpy(g, f, sizeof(f));
            for (int j = 0; j < v; ++j) {
                /*
                q[]为单调队列
                存储前个s(物品数量)中的最大价值对应的体积
                从头(hh)到尾(tt)单调递减

                hh为队首位置
                tt为队尾位置
                数值越大，表示位置越后面
                队首在队尾后面队列为空，即hh>tt时队列为空

                      最大s个元素的单调队列,新元素从tt方向进入
                hh--------------------------------------------tt
                      始终保持g[q[hh]]为当前最优

                k代表假设当前背包容量为k
                q[hh]为队首元素，当前
                g[]为f[i-1][]
                f[]为f[i][]

                */
                int hh = 0, tt = -1;
                for (int k = j; k <= m; k += v) {

                    //保险，存 i - 1的结果
                    // f[k] = g[k];

                    /*
                    队列往前走,跟上窗口,顺便去掉前一次的最优情况
                    */
                    if (hh <= tt && (k - q[hh]) / v > s) {
                        hh++;
                    }

                    /*
                    队内有值，队头就是当前窗口最优选择

                    g[q[hh] + k - q[hh] - (k - q[hh])] + (k - q[hh]) / v * w =
                    g[k - (k - g[hh])] + (k - q[hh]) / v * w
                    相当于在体积k, 选择第i种物品，且选择了 (k - q[hh]) / v 个
                    */
                    if (hh <= tt) {
                        f[k] = max(f[k], g[q[hh]] + (k - q[hh]) / v * w);
                    }

                    /*
                    f[j + kv] - kw =
                    max(f[j] , f[j + v] - w, f[j + 2v] - 2w , ... , f[j + kv] - kw) , k <= s

                    若队尾元素小于当前元素，则队尾元素出队；
                    若队内一个元素比当前元素小，则该元素一定不会被用到（单调队列思想）

                    g[q[tt]] - (q[tt] - j) / v * w
                    与
                    g[k] - (k - j) / v * w
                    间接表示队尾元素的价值和当前元素的价值
                    */
                    while (hh <= tt && g[q[tt]] - (q[tt] - j) / v * w <= g[k] - (k - j) / v * w) {
                        tt--;
                    }

                    //去除了比当前小的元素，保证队列里的元素都比当前元素大，入队
                    q[++tt] = k;
                }
            }
        }
        cout << f[m] << endl;
    }

  */
  def multipleBackPack(N: Int, V: Int, ele: Array[Array[Int]]): Int = {
    /*
    //brutal force
    val f = Array.fill(V)(0)
    for(i <- 0 to N){
      val vi = ele(i)(0)
      val wi = ele(i)(1)
      val si = ele(i)(2)
      for(j <- V to 0 by -1){
        for(k <- 1 to si if k * vi <= j)
        f(j) = math.max(f(j -  k * vi) + k * wi,f(j))
      }
    }
    */

    /*
    //binary solution
    val f = Array.fill(V)(0)
    case class Element(v:Int, w:Int)
    val elements = ArrayBuffer[Element]()
    //form log(s) types of choosing solution for ith good
    // si- 2^k + 1 >= 0
    // 2^k - 1 = sum(1 + 2 + 4 + 8 + 16 + 32 ....)
    for(i <- 0 to N){
      val wi = ele(i)(1)
      var si = ele(i)(2)
      //
      val vi = ele(i)(0)
      for(k <- 1 to si if k % 2 == 0 || k == 1){
        si -= k
        elements += Element(k * vi,k * wi)
      }
      if(si > 0)
        elements += Element(si * vi,si * wi)
    }

    // the residual and the sum can represent all the 1 ~ si numbers
    for(e <- elements){
      for(j <- V to 0 by -1 if j >= e.v)
        f(j) = Math.max(f(j),f(j - e.v) + e.w)
    }

    f(V)
    */

    //Monotonic queue
    /*
      f(j) = max(f(j - v) + w, f(j - 2v) + 2w , ..., f(j - s * v) + s * w)
      f(j + v) = max(f(j) , f(j - 2v) + 2w , ... , f(j - (s - 1) * v) + (s - 1) * w)

      From the basic solution we can learn that
      all the j can be obtained by step vi,
      i.e range 0 ~ V can be divided into vi subSets
      by module vi,
      also each subset is nothing to do with others

      such as 0 ~ 100 can be divided into 20 subSets
      by volume 20

      0 ~ 20
      0: 0 20 40 60 80 100
      1: 1 21 41 61 81
      2: 2 22 42 62 82
        ...

      For every element,
      we can iterate remainder range 0 ~ vi,
      for(j <- 0 to vi)

      For internal iteration
      we can fill to V from current remainder by step vi
      for(k <- j to V by vi)

      We can use monotonic queue to track the maximum value
      of the slide window which could be up to range [k - s * vi , k]
      then update the maximum value to f[k]

      i.e considering ith element and under volume k,
      also at most si numbers of ith element can be selected.
      we can get the maximum value

      how many vi from deque head : (k - deque.head) / vi

      Be careful to the comparing when something pretend to enqueue from tail

      f(j)    =     f(j)
      f(j+vi)  = max(f(j) +  wi,  f(j+vi))
      f(j+2vi) = max(f(j) + 2wi,  f(j+vi) +  wi, f(j+2vi))
      f(j+3vi) = max(f(j) + 3wi,  f(j+vi) + 2wi, f(j+2vi) + wi, f(j+3vi))
      ...
      Every time we increase corresponding wi in method max's formal parameters,
      that could be the trouble when we try to compare every 2 states.
      We can equally minus corresponding wi for every max's formal parameters,
      then plus that wi to the back for not to change the original intention.
      f(j)    =     f(j)
      f(j+vi)  = max(f(j), f(j+vi) - wi) + wi
      f(j+2vi) = max(f(j), f(j+vi) - wi, f(j+2vi) - 2wi) + 2wi
      f(j+3vi) = max(f(j), f(j+vi) - wi, f(j+2vi) - 2wi, f(j+3vi) - 3wi) + 3wi
      ...

      simple proof:
      f(j) = f(j)
      f(j+vi) = max(f(j), f(j+vi) - wi) + wi
      f(j+ 2 * vi) = max(f(j), f(j+vi) - wi, f(j+2 * vi) - 2 * wi) + 2 * wi

      f(j + 2 * vi) - 2 * wi = max(f(j) , max(f(j),f(j + vi) - wi) , f(j + 2 * vi) - 2 * wi)
                     = max(f(j), f(j + vi) - wi , f(j + 2 * vi) - 2 * wi)
      so:
      f(j + k * vi) - k * wi =
      max(f(j) , f(j + vi) - wi, f(j + 2 * vi) - 2 * wi , ... , f(j + k * vi) - k * wi) , k <= s

      Equivalent to

      f(j) =
      max(f(j) , f(j - vi) + wi, f(j - 2 * vi) + 2 * wi , ... , f(j - k * vi) + k * wi) , k <= s

      that makes it easy to compare every 2 states from back to front.

    */
    val f = Array.fill(V)(0)
    // g[] = f[i - 1][]
    val g = Array.fill(V)(0)
    // deque record the maximum value of current window
    var deque = ArrayBuffer[Int](Integer.MAX_VALUE)

    for (i <- 0 to N) {
      val vi = ele(i)(0)
      val wi = ele(i)(1)
      val si = ele(i)(2)
      var head = 0
      var tail = -1

      // remainder range 0 ~ vi
      for (j <- 0 to vi) {
        for (k <- j to V by vi) {
          //assign f[i - 1][] to g[]
          g(k) = f(k)

          //if the current deque length greater than si
          //deque move forward to match with window
          //also get rid of the previous maximum
          if (head <= tail && (k - deque(head) / vi > si)) {
            head += 1
            deque = deque.slice(head, tail + 1)
          }

          //if current deque head exists(deque is not empty)
          //g(deque(head)) + (k - deque(head)) / vi * wi means
          //for volume k, select ith element for (k - deque(head)) / vi times
          //i.e g(deque(head)) + (k - deque(head)) / vi * wi
          // = g(deque(head) + k - deque(head) - (k - deque(head))) + (k - deque(head)) / vi * wi
          // = g(k - (k - deque(head))) + (k - deque(head)) / vi * wi
          if (head <= tail)
            f(k) = Math.max(f(k), g(deque(head)) + (k - deque(head)) / vi * wi)

          //if current volume k can lead to a better value
          //get rid of the last one of deque
          while (head <= tail
            &&
            g(deque(head)) - (k - deque(head)) / vi * wi
              <
              g(k) - (k - j) / vi * wi
          ) {
            tail -= 1
            deque = deque.slice(head, tail + 1)
          }
          //until find a volume in deque that better than current k
          //insert that current k to the last position
          deque(tail) = k
        }
      }
    }

    f(V)
  }


  //混合背包问题
  /*
   有 N 种物品和一个容量是 V 的背包。

   物品一共有三类：

   第一类物品只能用1次（01背包）；
   第二类物品可以用无限次（完全背包）；
   第三类物品最多只能用 si 次（多重背包）；
   每种体积是 vi，价值是 wi。

   求解将哪些物品装入背包，可使物品体积总和不超过背包容量，且价值总和最大。
   输出最大价值。

   输入格式
   第一行两个整数，N，V，用空格隔开，分别表示物品种数和背包容积。

   接下来有 N 行，每行三个整数 vi,wi,si，用空格隔开，分别表示第 i 种物品的体积、价值和数量。

   si=−1 表示第 i 种物品只能用1次；
   si=0 表示第 i 种物品可以用无限次；
   si>0 表示第 i 种物品可以使用 si 次；

   si > 0 拆分成 log(si)份表示，转变为01问题

  */
  def mixBackPack(N: Int, V: Int, ele: Array[Array[Int]]): Int = {
    val f = Array.fill(V)(0)
    case class Good(kind: Int, volume: Int, value: Int)
    val goods = ArrayBuffer[Good]()

    //encapsulate Good
    for (i <- 0 until N) {
      val vi = ele(i)(0)
      val wi = ele(i)(1)
      var si = ele(i)(2)
      if (si < 0)
        goods += Good(-1, vi, wi)
      else if (si == 0)
        goods += Good(0, vi, wi)
      else {
        // si - (2^k - 1) and range (1 ~ 2^k)
        // can represent all the possibility of si choices
        for (k <- 1 to si if k % 2 == 0 || k == 1) {
          goods += Good(k, vi, wi)
          si -= k
        }
        if (si > 0)
          goods += Good(si, vi, wi)
      }
    }

    for (g <- goods) {
      val kind = g.kind
      val v = g.volume
      val w = g.value
      // multiple backpack problem trans to 01-backpack problem
      if (kind < 0) {
        for (j <- V to v by -1)
          f(j) = Math.max(f(j), f(j - v) + w)
      }
      else if (kind == 0) {
        /*
          for(j <- j - v to V)
             f(j) = Math.max(f[j],f[j - v] + w)
        */
        for (j <- V to v by -1) {
          for (k <- 1 to Integer.MAX_VALUE if k * v <= V)
            f(j) = Math.max(f(j), f(j - k * v) + k * w)
        }
      }
    }

    f(V)
  }


  //二维费用的背包问题
  /*
    有 N 件物品和一个容量是 V 的背包，背包能承受的最大重量是 M。

    每件物品只能用一次。体积是 vi，重量是 mi，价值是 wi。

    求解将哪些物品装入背包，可使物品总体积不超过背包容量，
    总重量不超过背包可承受的最大重量，且价值总和最大。
    输出最大价值。

    0<N≤1000
    0<V,M≤100
    0<vi,mi≤100
    0<wi≤1000

    体积  质量
    f[i][j]



  */
  def two_dimensionalBackPack(N: Int, V: Int, M: Int, ele: Array[Array[Int]]): Int = {
    val f = Array.ofDim[Int](V, M)
    for (i <- 0 to N) {
      val vi = ele(i)(0)
      val mi = ele(i)(0)
      val wi = ele(i)(2)

      for (j <- V to vi by -1) {
        for (k <- M to mi by -1)
          f(j)(k) = Math.max(f(j)(j), f(j - vi)(k - mi) + wi)
      }
    }

    f(V)(M)
  }


  //分组背包问题
  /*
   有 N 组物品和一个容量是 V 的背包。

   每组物品有若干个，同一组内的物品最多只能选一个。
   每件物品的体积是 vij，价值是 wij，其中 i 是组号，j 是组内编号。

   求解将哪些物品装入背包，可使物品总体积不超过背包容量，且总价值最大。

   输出最大价值。

   输入格式
   第一行有两个整数 N，V，用空格隔开，分别表示物品组数和背包容量。

   接下来有 N 组数据：

   每组数据第一行有一个整数 Si，表示第 i 个物品组的物品数量；
   每组数据接下来有 Si 行，每行有两个整数 vij,wij，用空格隔开，分别表示第 i 个物品组的第 j 个物品的体积和价值；
   输出格式
   输出一个整数，表示最大价值。

   数据范围
   0<N,V≤100
   0<Si≤100
   0<vij,wij≤100

   f[i][j]  前i组物品在空间j下的最大

   for(int i = 0;i < n; i ++){
    for(int j = m ; j >= v; j--)
      // s + 1 种决策
      f[j] = max(f[j],f[j - v[0]) + w[0], f[j - v[1]] + w[1] , ...... , f[j - v[s - 1]] + w[s - 1]

   }



  */
  def classifyBackPack(N: Int, V: Int, ele: Array[Array[Int]]): Int = {
    val f = Array.fill(V)(0)
    for (i <- 0 to N) {
      val si = ele(i)(0)
      val elements = ele(i).splitAt(1)._2
      val elementBuffer = ArrayBuffer[(Int, Int)]()
      for (k <- elements.indices if k % 2 == 1) {
        val vij = elements(k - 1)
        val wij = elements(k)
        elementBuffer += (vij -> wij)
      }

      for (e <- elementBuffer) {
        val vij = e._1
        val wij = e._2
        for (j <- V to vij by -1)
          f(j) = Math.max(f(j), f(j - vij) + wij)
      }
    }

    f(V)
  }


  //背包问题求方案数
  /*
    有 N 件物品和一个容量是 V 的背包。每件物品只能使用一次。

    第 i 件物品的体积是 vi，价值是 wi。

    求解将哪些物品装入背包，可使这些物品的总体积不超过背包容量，且总价值最大。

    输出 最优选法的方案数。注意答案可能很大，请输出答案模 109+7 的结果。

    输入格式
    第一行两个整数，N，V，用空格隔开，分别表示物品数量和背包容积。

    接下来有 N 行，每行两个整数 vi,wi，用空格隔开，分别表示第 i 件物品的体积和价值。

    输出格式
    输出一个整数，表示 方案数 模 109+7 的结果。

    数据范围
    0<N,V≤1000
    0<vi,wi≤1000

    f[V] 当前体积下最优选择
    g[V] 当前体积下最优选择的方案数

    01背包更新 f[j] 的同时更新g[j]
    注意mod，以及最佳方案下可能有不同体积

  */
  def pathNumOfBestSolutionBackPack(N: Int, V: Int, ele: Array[Array[Int]]): Int = {
    val mod = 1e9 + 7
    val f = Array.fill(V)(Integer.MIN_VALUE)
    val g = Array.fill(V)(1) //default path number to reach every volume

    for (i <- ele.indices) {
      val vi = ele(i)(0)
      val wi = ele(i)(1)
      for (j <- V to vi by -1) {
        val curOptimal = Math.max(f(j), f(j - vi) + wi)
        var curNum = 0.0
        if (curOptimal == f(j)) curNum += g(j)
        if (curOptimal == f(j - vi) + wi) curNum += g(j - vi)
        if (curNum >= mod) curNum %= mod
        f(j) = curOptimal
        g(j) = curNum.toInt
      }
    }

    val maxWorth = f.foldLeft(Integer.MIN_VALUE)(math.max)
    var res = 0.0
    for (i <- 0 to V) {
      if (maxWorth == f(i))
        res += g(i)
      if (res >= mod)
        res %= mod
    }

    res.toInt
  }


  //求背包问题的具体方案
  /*
     有 N 件物品和一个容量是 V 的背包。每件物品只能使用一次。

     第 i 件物品的体积是 vi，价值是 wi。

     求解将哪些物品装入背包，可使这些物品的总体积不超过背包容量，且总价值最大。

     输出 字典序最小的方案。这里的字典序是指：所选物品的编号所构成的序列。物品的编号范围是 1…N。

     输入格式
     第一行两个整数，N，V，用空格隔开，分别表示物品数量和背包容积。

     接下来有 N 行，每行两个整数 vi,wi，用空格隔开，分别表示第 i 件物品的体积和价值。

     输出格式
     输出一行，包含若干个用空格隔开的整数，表示最优解中所选物品的编号序列，且该编号序列的字典序最小。

     物品编号范围是 1…N。

     数据范围
     0<N,V≤1000
     0<vi,wi≤1000

     求完所有后反推，只能二维数组，必须记录下第i个物品选择与否

     物品从前往后贪心，能选则选，字典序小


  */
  def getTheExactPathOfBackPack(N:Int,V:Int,ele:Array[Array[Int]]):Array[Int] = {
    val f = Array.fill(N,V)(0)
    //get the solution of 01-BackPack
    for(i <- 1 to N){
      for(j <- 1 to V){
       val vi = ele(i)(0)
       val wi = ele(i)(1)
       f(i)(j) = math.max(f(i)(j),f(i - 1)(j - vi) + wi)
      }
    }

    // greedy iterating from begin ,
    // vol record the volume of current path
    // whether choose (i + 1)th element or not
    // has no effect on the optimal path
    // choose ith element for a smaller Lexicographic order
    val res = ArrayBuffer[Int]()
    var vol = V
    for(i <- 1 to N){
      val vi = ele(i)(0)
      val wi = ele(i)(1)
      if(f(i)(vol) == f(i + 1)(vol - vi) + wi) {
        res += i
        vol -= vi
      }
    }

    res.toArray
  }


  //有依赖的背包问题
  /*
    有 N 个物品和一个容量是 V 的背包。

    物品之间具有依赖关系，且依赖关系组成一棵树的形状。如果选择一个物品，则必须选择它的父节点。

              1
            /   \
           2     3
         /  \
        4    5

    如果选择物品5，则必须选择物品1和2。这是因为2是5的父节点，1是2的父节点。

    每件物品的编号是 i，体积是 vi，价值是 wi，依赖的父节点编号是 pi。物品的下标范围是 1…N。

    求解将哪些物品装入背包，可使物品总体积不超过背包容量，且总价值最大。

    输出最大价值。

    输入格式
    第一行有两个整数 N，V，用空格隔开，分别表示物品个数和背包容量。

    接下来有 N 行数据，每行数据表示一个物品。
    第 i 行有三个整数 vi,wi,pi，用空格隔开，分别表示物品的体积、价值和依赖的物品编号。
    如果 pi=−1，表示根节点。 数据保证所有物品构成一棵树。

    输出格式
    输出一个整数，表示最大价值。

    数据范围
    1≤N,V≤100
    1≤vi,wi≤100
    父节点编号范围：

    内部结点：1≤pi≤N;
    根节点 pi=−1;

    由下往上遍历
    f[i][j] 选择节点i 在空间j下 节点i对应子树 的最大价值

    树型DP + 分组背包
    从上往下递归元素，每考虑一个节点，
    先考虑它之下的所有子节点在不同体积下的最大价值
    转换为分组背包问题，每颗子树的不同体积归类于当前节点下的物品组(物品组包括不选择子树只选择自身)

    循环物品 => 循环体积 => 循环物品组里的元素(当前子节点用哪个体积)


  */
  def backPackWithDependency(N: Int, V: Int, ele: Array[Array[Int]]): Int = {

    var root = 0
    val relation = Array.fill(N)(Array[Int]())
    val f = Array.fill(N, V)(0)
    for (i <- ele.indices) {
      val pi = ele(i)(2)
      //generate the relation
      if (pi == -1) root = pi
      relation(pi) = relation(pi) :+ i
    }

    def dfs(idx: Int): Unit = {
      val vi = ele(idx)(0)
      val wi = ele(idx)(1)
      val children = relation(idx)
      for (son <- children) {
        //recursively down to the bottom , then up ,
        //for getting current subtrees's results
        dfs(son)
        //yet not choose current node , just current son's result
        for (j <- V - vi to 0 by -1)
          for (k <- 0 to j)
          //select the optimal result under current volume j
            f(idx)(j) = Math.max(f(idx)(j), f(idx)(j - k) + f(son)(k))
      }

      //if down to subtrees , current node must be chosen and calculated
      for (t <- V to vi by -1) f(idx)(t) = f(idx)(t - vi) + wi
      //impossible for iterate subtrees , because current node must be chosen
      for (t <- 0 until vi) f(idx)(t) = 0
    }

    dfs(root)
    f(root)(V)
  }


}
