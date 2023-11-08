
// From https://www.movable-type.co.uk/scripts/sha1.html
class Sha1{static hash(t,e){const r=Object.assign({msgFormat:"string",outFormat:"hex"},e);switch(r.msgFormat){default:case"string":t=function(t){try{return(new TextEncoder).encode(t,"utf-8").reduce((t,e)=>t+String.fromCharCode(e),"")}catch(e){return unescape(encodeURIComponent(t))}}(t);break;case"hex-bytes":t=function(t){const e=t.replace(" ","");return""==e?"":e.match(/.{2}/g).map(t=>String.fromCharCode(parseInt(t,16))).join("")}(t)}const o=[1518500249,1859775393,2400959708,3395469782],a=[1732584193,4023233417,2562383102,271733878,3285377520],n=(t+=String.fromCharCode(128)).length/4+2,c=Math.ceil(n/16),s=new Array(c);for(let e=0;e<c;e++){s[e]=new Array(16);for(let r=0;r<16;r++)s[e][r]=t.charCodeAt(64*e+4*r+0)<<24|t.charCodeAt(64*e+4*r+1)<<16|t.charCodeAt(64*e+4*r+2)<<8|t.charCodeAt(64*e+4*r+3)<<0}s[c-1][14]=8*(t.length-1)/Math.pow(2,32),s[c-1][14]=Math.floor(s[c-1][14]),s[c-1][15]=8*(t.length-1)&4294967295;for(let t=0;t<c;t++){const e=new Array(80);for(let r=0;r<16;r++)e[r]=s[t][r];for(let t=16;t<80;t++)e[t]=Sha1.ROTL(e[t-3]^e[t-8]^e[t-14]^e[t-16],1);let r=a[0],n=a[1],c=a[2],h=a[3],l=a[4];for(let t=0;t<80;t++){const a=Math.floor(t/20),s=Sha1.ROTL(r,5)+Sha1.f(a,n,c,h)+l+o[a]+e[t]>>>0;l=h,h=c,c=Sha1.ROTL(n,30)>>>0,n=r,r=s}a[0]=a[0]+r>>>0,a[1]=a[1]+n>>>0,a[2]=a[2]+c>>>0,a[3]=a[3]+h>>>0,a[4]=a[4]+l>>>0}for(let t=0;t<a.length;t++)a[t]=("00000000"+a[t].toString(16)).slice(-8);const h="hex-w"==r.outFormat?" ":"";return a.join(h)}static f(t,e,r,o){switch(t){case 0:return e&r^~e&o;case 1:return e^r^o;case 2:return e&r^e&o^r&o;case 3:return e^r^o}}static ROTL(t,e){return t<<e|t>>>32-e}}

export const sha1 =
string =>
{
  return Sha1.hash(string);
}
