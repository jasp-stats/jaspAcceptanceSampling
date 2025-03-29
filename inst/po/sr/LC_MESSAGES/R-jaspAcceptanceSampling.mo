��    O      �  k         �  $   �  E   �     $  $   D  
   i  _   t  !   �  $   �          '     ?     X     o  @   �  W   �  '   !	     I	     Z	     p	     �	     �	  C   �	  1   �	  1   .
  1   `
     �
     �
     �
  J   �
  <     S   V  �   �  �   v     	     !  G   ?  b   �  e   �  i   P     �  Y   �     4  4   K  $   �  	   �     �  y   �  z   C     �     �     �     �  ?     c   O  E   �  V   �     P     c     j     v     �     �     �  7   �  ~   �  3   w     �  y   �  n   :     �     �  C   �  s     {   �  g        k  a   q     �  �  �  I   �  N     (   _  G   �  
   �  �   �  =   �  C   �       4   7  +   l  +   �  :   �  k   �  �   k  ;   �  &   7  4   ^  0   �  (   �  2   �  �      a   �  a     a   o  0   �  (      *   +   �   V   k   �   �   N!  .  �!    #  2   *$  >   ]$  �   �$  �   0%  �   �%  �   �&  8   >'  �   w'  4   (  i   =(  C   �(     �(  +   )  �   /)  �   �)  8   �*  8   �*     �*  '   +  g   =+  �   �+  d   B,  �   �,  (   5-     ^-     k-     �-  6   �-  A   �-     .  k   :.  �   �.  Y   |/  6   �/  �   0  �   �0  :   �1     �1  �   �1  �   }2  �   T3  �   :4     �4  �   5     �5        4   /                       7       K   <         ;   O   A   #   N   8   	       J   >      -       
             2   +           B   "      E   C          !         5       $       1   )       L   F       G   ?                          *   :   &   (   9   D       3                  .                        %   @          '   I   M            =                    6       ,   H      0       (Incoming) Proportion Non-conforming 1 - α (Producer's risk) has to be greater than β (consumer's risk). <u>Decision:</u> <b>%s</b> lot. AOQ (Average Outgoing Quality) Curve AOQL: %.3f AQL (Acceptable Quality Level) value should be lower than RQL (Rejectable Quality Level) value. ASN (Average Sample Number) Curve ATI (Average Total Inspection) Curve Acc. Number Accept or Reject Lot %s Acceptance Probabilities Acceptance Probability Acceptance Sampling Plan Acceptance number (c) cannot be larger than the sample size (n). Acceptance numbers (c) are cumulative, so they need to be in a non-decreasing sequence. Acceptance probabilities at AQL and RQL Actual P(accept) Analyze Variable Plan Average Outgoing Quality Average Sample Number Average Total Inspection Can not accept or reject lot: sample size has to be greater than 1. Can not calculate AOQ. Check the plan parameters. Can not calculate ASN. Check the plan parameters. Can not calculate ATI. Check the plan parameters. Create Variable Plan Critical Distance (k) Cum. Sample Size Cumulative sample size (n1+n2+...) cannot be larger than the lot size (N). Current plan <b>CAN %s</b> meet the specified risk point(s). Final rejection number (r) needs to be 1 more than the final acceptance number (c). For all stages except the last stage, rejection numbers (r) have to be at at least 2 greater than the acceptance numbers (c).

                                   Else, subsequent stages become redundant. For hypergeometric distribution, N * proportion non-conforming should all be integer values.

Check the values of N and proportion non-conforming. Generated Sampling Plan Historical Standard Deviation If historical standard deviation is unknown, sample size has to be > 1. If the number of defective items out of %1$d sampled is <= %2$d, accept the lot. Reject otherwise. Lot size (N = %.0f) cannot be smaller than the sample size (n = %.0f) of the generated variable plan. Lot size (N = %1$.0f) cannot be smaller than the sample size (n = %2$.0f) of the generated variable plan. Lower Specification Limit (LSL) Lower limit for proportion non-conforming items needs to be smaller than the upper limit. Multiple Sampling Plan No valid values found in the plan. Check the inputs. OC (Operating Characteristics) Curve P(accept) Probability of Acceptance Probability of acceptance (%.3f) at AQL (%.3f) is <b>lower</b> than the required probability of acceptance (%.3f) at AQL. Probability of acceptance (%.3f) at RQL (%.3f) is <b>higher</b> than the required probability of acceptance (%.3f) at RQL. Prop. Non-conforming Proportion Non-conforming Rej. Number Rejection Probability Rejection number (r) cannot be larger than the sample size (n). Rejection number (r) for every stage has to be larger than the corresponding acceptance number (c). Rejection number (r) has to be larger than the acceptance number (c). Rejection numbers (r) are cumulative, so they need to be in a non-decreasing sequence. Required P(accept) Sample Sample Mean Sample Size Sample Standard Deviation Sample mean is invalid. Sample size Sample size (n) cannot be larger than the lot size (N). Sample size has to be <b>> 1</b> if <b>both</b> LSL and USL are provided, and historical standard deviation is <b>unknown</b>. Sample standard deviation has to be greater than 0. Single Sampling Plan Step size for proportion non-conforming items needs to be smaller than the difference between the upper and lower limits. Step size of 0 is allowed only if the lower and upper limits of proportion non-conforming items are identical. Upper Specification Limit (USL) Value Variable Sampling Plan (Standard deviation assumed to be <b>%s</b>) Variable plan generated for the current quality constraints has an invalid k value. Modify the quality constraints. Variable plan generated for the current quality constraints has an invalid sample size (n). Modify the quality constraints. Variable plan generated for the current quality constraints is invalid. Modify the quality constraints. Z.LSL Z.LSL = (mean - LSL) / historical standard deviation

Accept lot if Z.LSL >= k, otherwise reject. Z.USL Project-Id-Version: jaspAcceptanceSampling 0.19.2
PO-Revision-Date: 2025-03-23 20:15+0000
Last-Translator: Marko Bojović <bojovicmarko@outlook.com>
Language-Team: Serbian <https://hosted.weblate.org/projects/jasp/jaspacceptancesampling-r/sr/>
Language: sr
MIME-Version: 1.0
Content-Type: text/plain; charset=UTF-8
Content-Transfer-Encoding: 8bit
Plural-Forms: nplurals=3; plural=n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2;
X-Generator: Weblate 5.11-dev
 (Долазна) пропорција неисправних ставки 1 - α (Producer's risk) мора бити већи од β (consumer's risk). <u>Одлука:</u> <b>%s</b> низа. AOQ (просечног излазног квалитета) крива AOQL: %.3f AQL (ниво прихватљивог квалитета) вредност треба да буде нижа од RQL (ниво одбијеног квалитета) вредности. ASN (просечног броја узорака) крива ATI (просечне укупне инспекције) крива Прихваћени број Прихвати или одбаци серију %s Вероватноће прихватања Вероватноћа прихватања План узорковања прихватљивости Број прихватања (ц) не може бити већи од величине узорка (н). Бројеви прихватања (ц) су кумулативни, тако да морају бити у неопадајућем низу. Вероватноће прихватања на AQL и RQL Стварни P(прихватање) Анализирајте план варијабле Просечни излазни квалитет Просечан број узорака Просечна укупна инспекција Не може се прихватити или одбити серију: величина узорка мора бити већа од 1. Није могуће израчунати AOQ. Проверите параметре плана. Није могуће израчунати ASN. Проверите параметре плана. Није могуће израчунати ATI. Проверите параметре плана. Креирајте променљиви план Критично растојање (к) Укупна величина узорка Кумулативна величина узорка (н1+н2+...) не може бити већа од величине серије (Н). Тренутни план <b>МОЖЕ %s</b> задовољити наведене тачке ризика. Коначни број одбијања (р) треба да буде 1 већи од коначног броја прихватања (ц). За све фазе осим последње фазе, бројеви одбијања (р) морају бити најмање 2 већи од бројева прихватања (ц).

                                   У супротном, наредне фазе постају сувишне. За хипергеометријску дистрибуцију, Н * пропорција неусаглашених треба да буду целобројне вредности.

Проверите да су вредности Н и пропорција неусаглашени. Генерисани план узорковања Историјска стандардна девијација Ако је историјска стандардна девијација непозната, величина узорка мора бити > 1. Ако је број неисправних артикала од %1$d узрокованих <= %2$d, прихватите партију. Одбијте у супротном. Величина серије (N = %.0f) не може бити мања од величине узорка (n = %.0f) генерисаног плана променљиве. Величина низа (N = %1$.0f) не може бити мања од величине узорка (n = %2$.0f) генерисаног плана варијабли. Доња граница спецификације (LSL) Доња граница за пропорцију неисправних ставки мора бити мања од горње границе. План вишеструког узорковања Нису пронађене важеће вредности у плану. Проверите уносе. OC (оперативних карактеристика) крива P(прихватити) Вероватноћа прихватања Вероватноћа прихватања (%.3f) на AQL (%.3f) је <b>нижа</b> од захтеване вероватноће прихватања (%.3f) на AQL. Вероватноћа прихватања (%.3f) на RQL (%.3f) је <b>већа</b> од захтеване вероватноће прихватања (%.3f) на RQL. Пропорција неисправних ставки Пропорција неисправних ставки Одбачени број Вероватноћа одбијања Број одбијања (р) не може бити већи од величине узорка (н). Број одбијања (р) за сваки степен мора бити већи од одговарајућег броја прихватања (ц). Одбијени број (р) мора бити већи од броја прихватања (ц). Бројеви одбијања (r) су кумулативни, тако да треба да буду у неопадајућем низу. Потребна P(прихватање) Узорак Средњи узорак Величина узорка Стандардна девијација узорка Средња вредност узорка је неважећа. Величина узорка Величина узорка (н) не може бити већа од величине серије (Н). Величина узорка мора бити <b>> 1</b> ако <b> и </b> LSL и USL су обезбеђене, а историјска стандардна девијација је <b>непозната</b>. Стандардна девијација узорка мора бити већа од 0. План појединачног узорковања Величина корака за пропорције неусаглашених ставки треба да буде мања од разлике између горње и доње границе. Величина корака од 0 је дозвољена само ако су доња и горња граница пропорција неусклађених ставки идентичне. Горња граница спецификације (USL) Вресност Варијабилни план узорковања (претпоставља се да је стандардна девијација <b>%s</b>) Генерисани план променљиве за тренутна ограничења квалитета има неважећу k вредност. Измените ограничења квалитета. Генерисани план променљиве за тренутна ограничења квалитета има неважећу величину узорка (n). Измените ограничења квалитета. Генерисани план променљиве за тренутна ограничења квалитета је неважећи. Измените ограничења квалитета. Z.LSL Z.LSL = (аритметичка средина - LSL) / историјска стандардна девијација

Прихватите низ ако је Z.LSL >= k, у супротном одбаци. Z.USL 