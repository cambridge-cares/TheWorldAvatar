
import { Assets } from 'io/config/assets';
import Image from 'next/image';
import styles from './loader.module.css';

export default function Loader() {
  return <div className={styles.loadingContainer}>
    <Image src= {Assets.LOADING}
      width={500}
      height={500} 
      alt ="Loading animation"/>
    <h1>Loading, please wait...</h1>
  </div>
}