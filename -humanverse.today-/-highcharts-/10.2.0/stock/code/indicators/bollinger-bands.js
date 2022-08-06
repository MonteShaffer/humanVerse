/*
 Highstock JS v10.2.0 (2022-07-05)

 Indicator series type for Highcharts Stock

 (c) 2010-2021 Pawe Fus

 License: www.highcharts.com/license
*/
(function(a){"object"===typeof module&&module.exports?(a["default"]=a,module.exports=a):"function"===typeof define&&define.amd?define("highcharts/indicators/bollinger-bands",["highcharts","highcharts/modules/stock"],function(h){a(h);a.Highcharts=h;return a}):a("undefined"!==typeof Highcharts?Highcharts:void 0)})(function(a){function h(a,f,c,h){a.hasOwnProperty(f)||(a[f]=h.apply(null,c),"function"===typeof CustomEvent&&window.dispatchEvent(new CustomEvent("HighchartsModuleLoaded",{detail:{path:f,module:a[f]}})))}
a=a?a._modules:{};h(a,"Stock/Indicators/MultipleLinesComposition.js",[a["Core/Series/SeriesRegistry.js"],a["Core/Utilities.js"]],function(a,f){var c=a.seriesTypes.sma.prototype,h=f.defined,p=f.error,t=f.merge,k;(function(a){function f(b){return"plot"+b.charAt(0).toUpperCase()+b.slice(1)}function A(b,g){var a=[];(b.pointArrayMap||[]).forEach(function(b){b!==g&&a.push(f(b))});return a}function d(){var b=this,g=b.linesApiNames,a=b.areaLinesNames,e=b.points,d=b.options,v=b.graph,x={options:{gapSize:d.gapSize}},
n=[],m=A(b,b.pointValKey),l=e.length,k;m.forEach(function(b,a){for(n[a]=[];l--;)k=e[l],n[a].push({x:k.x,plotX:k.plotX,plotY:k[b],isNull:!h(k[b])});l=e.length});if(b.userOptions.fillColor&&a.length){var w=m.indexOf(f(a[0]));w=n[w];a=1===a.length?e:n[m.indexOf(f(a[1]))];m=b.color;b.points=a;b.nextPoints=w;b.color=b.userOptions.fillColor;b.options=t(e,x);b.graph=b.area;b.fillGraph=!0;c.drawGraph.call(b);b.area=b.graph;delete b.nextPoints;delete b.fillGraph;b.color=m}g.forEach(function(a,g){n[g]?(b.points=
n[g],d[a]?b.options=t(d[a].styles,x):p('Error: "There is no '+a+' in DOCS options declared. Check if linesApiNames are consistent with your DOCS line names."'),b.graph=b["graph"+a],c.drawGraph.call(b),b["graph"+a]=b.graph):p('Error: "'+a+" doesn't have equivalent in pointArrayMap. To many elements in linesApiNames relative to pointArrayMap.\"")});b.points=e;b.options=d;b.graph=v;c.drawGraph.call(b)}function v(b){var a,d=[];b=b||this.points;if(this.fillGraph&&this.nextPoints){if((a=c.getGraphPath.call(this,
this.nextPoints))&&a.length){a[0][0]="L";d=c.getGraphPath.call(this,b);a=a.slice(0,d.length);for(var e=a.length-1;0<=e;e--)d.push(a[e])}}else d=c.getGraphPath.apply(this,arguments);return d}function x(b){var a=[];(this.pointArrayMap||[]).forEach(function(d){a.push(b[d])});return a}function w(){var b=this,a=this.pointArrayMap,d=[],e;d=A(this);c.translate.apply(this,arguments);this.points.forEach(function(g){a.forEach(function(a,c){e=g[a];b.dataModify&&(e=b.dataModify.modifyValue(e));null!==e&&(g[d[c]]=
b.yAxis.toPixels(e,!0))})})}var k=[],B=["bottomLine"],y=["top","bottom"],z=["top"];a.compose=function(b){if(-1===k.indexOf(b)){k.push(b);var a=b.prototype;a.linesApiNames=a.linesApiNames||B.slice();a.pointArrayMap=a.pointArrayMap||y.slice();a.pointValKey=a.pointValKey||"top";a.areaLinesNames=a.areaLinesNames||z.slice();a.drawGraph=d;a.getGraphPath=v;a.toYData=x;a.translate=w}return b}})(k||(k={}));return k});h(a,"Stock/Indicators/BB/BBIndicator.js",[a["Stock/Indicators/MultipleLinesComposition.js"],
a["Core/Series/SeriesRegistry.js"],a["Core/Utilities.js"]],function(a,f,c){var h=this&&this.__extends||function(){var a=function(c,d){a=Object.setPrototypeOf||{__proto__:[]}instanceof Array&&function(a,d){a.__proto__=d}||function(a,d){for(var c in d)d.hasOwnProperty(c)&&(a[c]=d[c])};return a(c,d)};return function(c,d){function f(){this.constructor=c}a(c,d);c.prototype=null===d?Object.create(d):(f.prototype=d.prototype,new f)}}(),p=f.seriesTypes.sma,t=c.extend,k=c.isArray,u=c.merge;c=function(a){function c(){var d=
null!==a&&a.apply(this,arguments)||this;d.data=void 0;d.options=void 0;d.points=void 0;return d}h(c,a);c.prototype.init=function(){f.seriesTypes.sma.prototype.init.apply(this,arguments);this.options=u({topLine:{styles:{lineColor:this.color}},bottomLine:{styles:{lineColor:this.color}}},this.options)};c.prototype.getValues=function(a,c){var d=c.period,h=c.standardDeviation,p=a.xData,t=(a=a.yData)?a.length:0,y=[],z=[],b=[],g;if(!(p.length<d)){var v=k(a[0]);for(g=d;g<=t;g++){var e=p.slice(g-d,g);var r=
a.slice(g-d,g);var q=f.seriesTypes.sma.prototype.getValues.call(this,{xData:e,yData:r},c);e=q.xData[0];q=q.yData[0];for(var u=0,n=r.length,m=0;m<n;m++){var l=(v?r[m][c.index]:r[m])-q;u+=l*l}l=Math.sqrt(u/(n-1));r=q+h*l;l=q-h*l;y.push([e,r,q,l]);z.push(e);b.push([r,q,l])}return{values:y,xData:z,yData:b}}};c.defaultOptions=u(p.defaultOptions,{params:{period:20,standardDeviation:2,index:3},bottomLine:{styles:{lineWidth:1,lineColor:void 0}},topLine:{styles:{lineWidth:1,lineColor:void 0}},tooltip:{pointFormat:'<span style="color:{point.color}">\u25cf</span><b> {series.name}</b><br/>Top: {point.top}<br/>Middle: {point.middle}<br/>Bottom: {point.bottom}<br/>'},
marker:{enabled:!1},dataGrouping:{approximation:"averages"}});return c}(p);t(c.prototype,{areaLinesNames:["top","bottom"],linesApiNames:["topLine","bottomLine"],nameComponents:["period","standardDeviation"],pointArrayMap:["top","middle","bottom"],pointValKey:"middle"});a.compose(c);f.registerSeriesType("bb",c);"";return c});h(a,"masters/indicators/bollinger-bands.src.js",[],function(){})});
//# sourceMappingURL=bollinger-bands.js.map