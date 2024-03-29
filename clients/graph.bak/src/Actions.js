export const setTimeout0 =
f =>
() =>
{
  setTimeout(f, 0);
};

export const focusElementById =
id =>
() =>
{
  const el = document.getElementById(id);
  if (el) el.focus();
};

export const shuffle =
array =>
() =>
// from https://stackoverflow.com/a/2450976/4608364
{
  array = [...array];
  let currentIndex = array.length;
  while (currentIndex !== 0) {
    const randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex--;
    [array[currentIndex], array[randomIndex]] = [array[randomIndex], array[currentIndex]];
  }
  return array; 
}

export const download_f =
name => content => () =>
{
  // https://stackoverflow.com/a/18197341
  var $a = document.createElement('a');
  $a.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(content));
  $a.setAttribute('download', name);
  $a.style.display = 'none';
  document.body.appendChild($a);
  $a.click();
  document.body.removeChild($a);
};
