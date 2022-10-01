package limbo

import java.io.File

object FileTest {
  var fileStream:Stream[File] = ???
  def subDir(dir: File, _fileStream:Stream[File]):Iterable[File]= {
    if(_fileStream.isEmpty){
      val iter = dir.listFiles().filter(_.isFile()).filter(f => {
        val name = f.getName
        //val ret = regexFileter.pattern.matcher(name).find()
        var ret = ???
        ret
      })
      iter.foreach(f => _fileStream.:+(f))
    }
    //val num = config.getFileNum()
    val num:Int = ???
    if(num > 0){
      var (need,residual) = _fileStream.splitAt(num)
      fileStream = residual
      need.filter(f => f.exists())
    }
    else{
      val ret = _fileStream.filter(f => f.exists())
      fileStream = Stream.empty
      ret
    }
  }
}
